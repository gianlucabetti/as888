000100111128       //==============================================================
000200120510       // TRTCMAR - Cancella i rcd di TIMAL00F aventi i dati inputati nei filtri a video e il
000300120510       // progressivo = 0.
000400120405       //==============================================================
000500111128
000600111128       //--------------------------------------------------------------
000700120207       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800111128       //--------------------------------------------------------------
000900111128
001000111128     /*PRM  dbgview(*source)
001100111128     /*END
001200111128
001300111128       //--------------------------------------------------------------
001400120207       // Specifiche di controllo.                                     ?
001500111128       //--------------------------------------------------------------
001600111128
001700111128     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800120510     h dftactgrp(*no) bnddir('UBBNDDIR')
001900111128     h alwnull(*inputonly)
002000111128
002100111128       //--------------------------------------------------------------
002200120207       // Dichiarazione file.                                          ?
002300111128       //--------------------------------------------------------------
002400120215       // - Video
002500120510     fTRTCMAD   cf   e             workstn
002600120207     f                                     indds(IndDspF)
002700120207     f                                     infds(InfDspF)
002800111128
002900111128
003000111128       //--------------------------------------------------------------
003100120207       // Definizione costanti.                                        ?
003200111128       //--------------------------------------------------------------
003300120207
003400120207       // - Tasti funzionali a video
003500120207     d c_F01           c                   const(x'31')
003600120207     d c_F02           c                   const(x'32')
003700120207     d c_F03           c                   const(x'33')
003800120207     d c_F05           c                   const(x'35')
003900120207     d c_F06           c                   const(x'36')
004000120207     d c_F07           c                   const(x'37')
004100120207     d c_F08           c                   const(x'38')
004200120207     d c_F09           c                   const(x'39')
004300120207     d c_F10           c                   const(x'3A')
004400120207     d c_F12           c                   const(x'3C')
004500120207     d c_F13           c                   const(x'B1')
004600120207     d c_F14           c                   const(x'B2')
004700120207     d c_F15           c                   const(x'B3')
004800120207     d c_F16           c                   const(x'B4')
004900120207     d c_F17           c                   const(x'B5')
005000120207     d c_F18           c                   const(x'B6')
005100120207     d c_F19           c                   const(x'B7')
005200120207     d c_F20           c                   const(x'B8')
005300120207     d c_F21           c                   const(x'B9')
005400120207     d c_F22           c                   const(x'BA')
005500120207     d c_F23           c                   const(x'BB')
005600120207     d c_F24           c                   const(x'BC')
005700120207     d c_Enter         c                   const(x'F1')
005800120207     d c_RollDown      c                   const(x'F4')
005900120207     d c_RollUp        c                   const(x'F5')
006000111128
006100111128
006200111128       //--------------------------------------------------------------
006300120207       // Definizione schiere.                                         ?
006400111128       //--------------------------------------------------------------
006500111128
006600111128
006700111128       //--------------------------------------------------------------
006800120207       // Definizione aree dati.                                       ?
006900111128       //--------------------------------------------------------------
007000111128
007100111128
007200111128       //--------------------------------------------------------------
007300120207       // Definizione strutture dati.                                  ?
007400111128       //--------------------------------------------------------------
007500120510
007600120510
007700111128       //--------------------------------------------------------------
007800120207       // Definizione variabili globali.                               ?
007900111128       //--------------------------------------------------------------
008000111128
008100120208       // - Stringa SQL da eseguire
008200111128     d wSQl            s           1024    inz  varying
008300111128
008400120208       // - Parametri SQL
008500120207
008600120207       // - InfDS
008700120207     d InfDspF         ds
008800120207     d   dsp_aid             369    369a                                        AID byte
008900120207
009000120207       // - Indicatori su DspF
009100120207     d IndDspF         ds
009200120207        // - Indicatori di errore
009300120208     d  ErrMsg                        1n   overlay(IndDspF : 28)
009400120207     d  ErrGenerico                   1n   overlay(IndDspF : 99)
009500120404
009600120404     d KPJBA         e ds
009700120207
009800120207       // - Flags booleani
009900120207     d $Fine           s               n   inz(*off)
010000120510     d $InzW01         s               n   inz(*on)
010100120208     d ErrBlock        s               n   inz(*off)
010200120210     d Qcmd            s            500    inz
010300120207
010400120207       // - Campi associati al video
010500120510     d $Video          s              2    inz('W1')
010600120207
010700120207       // - Variabili di appoggio
010800120510     d wData08         s              8s 0
010900120405     d wX              s              5s 0
011000111128
011100111128       //--------------------------------------------------------------
011200120210       // Definizione procedure usate
011300111128       //--------------------------------------------------------------
011400120207
011500120210       // - Parametri API QCAPCMD (Process Commands)
011600120207      /copy qSysInc/qRpgleSrc,QCAPCMD
011700120207      /copy gaitrasrc/srcProtoPR,QCAPCMD
011800120510      /COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
011900120207
012000120210       // - Parametri gestione errori API
012100120207      /copy qSysInc/qRpgleSrc,QUSEC
012200120404
012300120510     d TRTCMAR         pr
012400120404     d  Arch                        502
012500120404
012600120510     d TRTCMAR         pi
012700120404     d  Arch                        502
012800111128
012900111128       //--------------------------------------------------------------
013000120208       // Definizione key-list.                                        ?
013100111128       //--------------------------------------------------------------
013200111128
013300111128
013400111128       //--------------------------------------------------------------
013500120208       // M A I N - L I N E                                            ?
013600111128       //--------------------------------------------------------------
013700111128
013800111128      /free
013900111128
014000120208       // - Operazioni iniziali
014100111128       exsr  sr_RoutInz;
014200111128
014300120207       // - Gestione video
014400120207       DOW  $Fine = *off;
014500120207         select;
014600120510           when  $Video = 'W1';
014700120510             exsr  sr_GesW01;
014800120207           other;
014900120207             $Fine = *on;
015000120207         endsl;
015100120207       ENDDO;
015200120207
015300120208       // - Operazioni finali
015400111128       exsr  sr_RoutEnd;
015500111128
015600111128       //--------------------------------------------------------------
015700120208       // Operazioni iniziali
015800111128       //--------------------------------------------------------------
015900111128       BEGSR sr_RoutInz;
016000111128
016100111128         *inLR = *on;
016200120207         IndDspF  = *off;
016300120404         KPJBA = Arch;
016400111128
016500120208         // - Impostazione opzioni per SQL
016600111128         exec SQL   set option   DynUsrPrf = *owner,
016700111128                                 CloSqlCsr = *endmod;
016800111128
016900111128
017000111128       ENDSR;
017100120207
017200120207       //--------------------------------------------------------------
017300120510       // Gestione videata W01
017400120207       //--------------------------------------------------------------
017500120510       BEGSR  sr_GesW01;
017600120207
017700120207         // - Inizializzazione videata
017800120510         if  $InzW01   = *on;
017900120510           exsr  sr_InzW01;
018000120510           $InzW01  = *off;
018100120207         endif;
018200120207
018300120207         // - Emissione videata
018400120510         exfmt  tcmaW01;
018500120207
018600120208         reset  ErrMsg;
018700120510         clear  W01msg;
018800120207
018900120207         SELECT;
019000120207
019100120207           // - F3=Fine
019200120207           WHEN  dsp_aid = c_F03;
019300120510             exsr sr_F03W01;
019400120207
019500120207           // - F12=Ritorno
019600120207           WHEN  dsp_aid = c_F12;
019700120510             exsr sr_F12W01;
019800120207
019900120207           // - Invio / F6=Conferma
020000120207           OTHER;
020100120510             exsr  sr_CtrW01;
020200120207             if  ErrGenerico = *on;
020300120207               leavesr;
020400120207             endif;
020500120207             if  dsp_aid = c_F06;
020600120510               exsr sr_F06W01;
020700120207             endif;
020800120207
020900120207         ENDSL;
021000120207
021100120207       ENDSR;
021200120207
021300120207       //--------------------------------------------------------------
021400120510       // Inizializzazione videata W01
021500120207       //--------------------------------------------------------------
021600120510       BEGSR  sr_InzW01;
021700120207
021800120207         // - Pulizia videata
021900120510         clear  tcmaW01;
022000120404
022100120404         // - impostazioni default
022200120510         w01DaOr = %dec(%date() : *EUR);
022300120207
022400120207       ENDSR;
022500120207
022600120207       //--------------------------------------------------------------
022700120510       // Gestione tasto funzionale F3 da videata W01                  ?
022800120510       // F3=Fine                                                      ?
022900120207       //--------------------------------------------------------------
023000120510       BEGSR  sr_F03W01;
023100120207
023200120207         // - Chiusura del programma
023300120207         $Fine = *on;
023400120207
023500120207       ENDSR;
023600120207
023700120207       //--------------------------------------------------------------
023800120510       // Gestione tasto funzionale F6 da videata W01
023900120208       // F6=Conferma
024000120207       //--------------------------------------------------------------
024100120510       BEGSR  sr_F06W01;
024200120405
024300120510         // Cancello i log con progressivo = 0
024400120510         exsr  DeleteLog0;
024500120510         if ErrBlock = *on;
024600120216           leavesr;
024700120216         endif;
024800120215
024900120510         // Esco dal pgm (solo se non ho errori da mostrare)
025000120208         if ErrMsg = *off;
025100120208           $Fine = *on;
025200120208         endif;
025300120207
025400120207       ENDSR;
025500120208
025600120207       //--------------------------------------------------------------
025700120510       // Gestione tasto funzionale F12 da videata W01
025800120208       // F12=Ritorno
025900120207       //--------------------------------------------------------------
026000120510       BEGSR  sr_F12W01;
026100120207
026200120207         // - Chiusura del programma
026300120207         $Fine = *on;
026400120207
026500120207       ENDSR;
026600120207
026700120207       //--------------------------------------------------------------
026800120510       // Controllo videata W01                                        ?
026900120207       //--------------------------------------------------------------
027000120510       BEGSR  sr_CtrW01;
027100120207
027200120207         IndDspF  = *off;
027300120208
027400120510         // se scritta, la data deve essere corretta
027500120510         if  W01DaOr>0;
027600120510           wData08 = %dec(%subst(
027700120510                    UBFMTDATE_Convert(
027800120510                     %editc(W01DaOr:'X'):
027900120510                     'DDMMYYYY':
028000120510                     'YYYYMMDD') :
028100120510                     1 : 8) : 8 : 0);
028200120510           if  wData08 = W01DaOr;
028300120510             W01msg = 'Data errata';
028400120510             ErrMsg = *on;
028500120510             ErrGenerico = *on;
028600120510             leavesr;
028700120510           else;
028800120510           // se la data � corretta, deve essere <= oggi
028900120510             if wData08 > %dec(%date() : *ISO);
029000120510               W01msg = 'Data > di oggi';
029100120510               ErrMsg = *on;
029200120510               ErrGenerico = *on;
029300120510               leavesr;
029400120510             endif;
029500120510           endif;
029600120510         endif;
029700120207
029800120207       ENDSR;
029900120208
030000120208       //--------------------------------------------------------------
030100120510       // Eseguo SQL di cancellazione
030200120208       //--------------------------------------------------------------
030300120510       BEGSR  DeleteLog0;
030400120208
030500120208         ErrBlock = *off;
030600120208
030700120208         // - Creazione stringa SQL da eseguire
030800120510         wSQL = 'delete from TIMAL00F where MALPROG=''0000000''';
030900120510         if W01FNam <> *blank;
031000120510           wSQL = %trim(wSQL) + ' and MALFNAM=''' + w01FNam + '''';
031100120510         endif;
031200120510         if W01UNam <> *blank;
031300120510           wSQL = %trim(wSQL) + ' and MALUNAM=''' + w01UNam + '''';
031400120510         endif;
031500120510         if W01UDta <> *blank;
031600120510           wSQL = %trim(wSQL) + ' and MALUDTA=''' + w01UDta + '''';
031700120510         endif;
031800120510         if W01DaOr <> 0;
031900120511           // trasformo la data YYYYMMDD in una data ISO e poi in un char, per confrontarla con
032000120511           // la parte data di MALDAOR
032100120511           wSQL = %trim(wSQL) + ' and substr(MALDAOR , 1 , 10)='''
032200120511                  + %char(%date(wData08)) + '''';
032300120510         endif;
032400120510
032500120510         exec sql  execute immediate :wSQL;
032600120510
032700120510         select;
032800120510         // errore
032900120510         when SQLCode < 0;
033000120510           exsr  sr_PrintErr;
033100120510           W01msg = 'Errore in cancellazione - vedi stampa errore';
033200120510           ErrMsg = *on;
033300120510           ErrGenerico = *on;
033400120510           ErrBlock = *on;
033500120510         // nessuna lettura
033600120510         when SQLCode = 100;
033700120510           W01msg = 'Nessun dato per i filtri immessi';
033800120510           ErrMsg = *on;
033900120510           ErrGenerico = *on;
034000120510           ErrBlock = *on;
034100120510         // fine corretta
034200120510         other;
034300120510           // non faccio nulla
034400120510         endsl;
034500120208
034600120208       ENDSR;
034700120215
034800120215       //--------------------------------------------------------------
034900120215       // Esecuzione del comando (gi� impostato)
035000120215       //--------------------------------------------------------------
035100120215       BEGSR  sr_ExecCmd;
035200120215
035300111128         clear Qcap0100;
035400111128         Qcabcsdh = *off;
035500111128         Qcapa    = *off;
035600111128         Qcacmdss = *off;
035700111128         Qcaerved = *allX'00';
035800111128
035900111128         clear Qusec;
036000111128         Qusbprv  = %size(Qusec);
036100111128
036200111128         ProcessCommands ( Qcmd : %len( %trimr( Qcmd ) ) : Qcap0100 :
036300111128                           %size(Qcap0100) : 'CPOP0100' : *omit :
036400111128                           0 : 0 : Qusec);
036500111128
036600120210         // - Stampa DUMP + JobLog  &  Chiusura *pgm  in caso di errore
036700111128         if  Qusei <> *blank;
036800111128           exsr  sr_PrintErr;
036900111128         endif;
037000111128
037100111128       ENDSR;
037200111128
037300111128       //--------------------------------------------------------------
037400120210       // Stampa segnalazione dell'errore rilevato
037500111128       //--------------------------------------------------------------
037600111128       BEGSR  sr_PrintErr;
037700111128
037800120210         // - Stampa del Dump
037900111128         Dump(A);
038000111128
038100120210         // - Stampa del Job-Log
038200111128         Qcmd = 'DSPJOBLOG job(*) output(*print)';
038300111128         exsr  sr_ExecCmd;
038400111128
038500111128       ENDSR;
038600111128
038700111128       //--------------------------------------------------------------
038800120208       // Operazioni finali
038900111128       //--------------------------------------------------------------
039000111128       BEGSR  sr_RoutEnd;
039100111128
039200120208         // - Uscita
039300111128         return;
039400111128
039500111128       ENDSR;
039600111128
039700111128      /end-free
