000100120423       //==============================================================
000200120423       //?Estrazione file Località Cappario.                           ?
000300120423       //==============================================================
000400120423
000500120423       //--------------------------------------------------------------
000600120423       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700120423       //--------------------------------------------------------------
000800120423
000900120423     /*PRM  dbgview(*source)
001000120423     /*END
001100120423
001200120423       //--------------------------------------------------------------
001300120423       //?Specifiche di controllo.                                     ?
001400120423       //--------------------------------------------------------------
001500120423
001600120423     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700120423     h dftactgrp(*no)
001800120423
001900120423       //--------------------------------------------------------------
002000120423       //?Dichiarazione file.                                          ?
002100120423       //--------------------------------------------------------------
002200120423
002300120423       // -?Video?
002400120423     fTRULCAPD  cf   e             workstn
002500120423     f                                     indds(IndDspF)
002600120423     f                                     infds(InfDspF)
002700120423
002800120423       //--------------------------------------------------------------
002900120423       //?Definizione costanti.                                        ?
003000120423       //--------------------------------------------------------------
003100120423
003200120423       // -?Tasti funzionali a video?
003300120423     d c_F01           c                   const(x'31')
003400120423     d c_F02           c                   const(x'32')
003500120423     d c_F03           c                   const(x'33')
003600120423     d c_F04           c                   const(x'34')
003700120423     d c_F05           c                   const(x'35')
003800120423     d c_F06           c                   const(x'36')
003900120423     d c_F07           c                   const(x'37')
004000120423     d c_F08           c                   const(x'38')
004100120423     d c_F09           c                   const(x'39')
004200120423     d c_F10           c                   const(x'3A')
004300120423     d c_F11           c                   const(x'3B')
004400120423     d c_F12           c                   const(x'3C')
004500120423     d c_F13           c                   const(x'B1')
004600120423     d c_F14           c                   const(x'B2')
004700120423     d c_F15           c                   const(x'B3')
004800120423     d c_F16           c                   const(x'B4')
004900120423     d c_F17           c                   const(x'B5')
005000120423     d c_F18           c                   const(x'B6')
005100120423     d c_F19           c                   const(x'B7')
005200120423     d c_F20           c                   const(x'B8')
005300120423     d c_F21           c                   const(x'B9')
005400120423     d c_F22           c                   const(x'BA')
005500120423     d c_F23           c                   const(x'BB')
005600120423     d c_F24           c                   const(x'BC')
005700120423     d c_Enter         c                   const(x'F1')
005800120423     d c_RollDown      c                   const(x'F4')
005900120423     d c_RollUp        c                   const(x'F5')
006000120423
006100120423       //--------------------------------------------------------------
006200120423       //?Definizione schiere.                                         ?
006300120423       //--------------------------------------------------------------
006400120423
006500120423       // -?Messaggi di errore?
006600120423     d $Msg            s             78    dim( 2)  ctdata  perrcd(1)
006700120423
006800120423       //--------------------------------------------------------------
006900120423       //?Definizione aree dati.                                       ?
007000120423       //--------------------------------------------------------------
007100120423
007200120423       // -?Dati utente?
007300120423     d §AzUte        e ds                  extname(AZUTE00F)
007400120423     d                                     dtaara
007500120423     d §DatiUte      e ds                  extname(dDatiUte)
007600120423     d                                     dtaara
007700120423
007800120423       //--------------------------------------------------------------
007900120423       //?Definizione strutture dati.                                  ?
008000120423       //--------------------------------------------------------------
008100120423
008200120423       // -?Status ds?
008300120423     d Status         sds
008400120423     d   SDSpgm          *proc
008500120423
008600120423       // -?InfDS?
008700120423     d InfDspF         ds
008800120423     d   dsp_aid             369    369a
008900120423
009000120423       // -?Indicatori su DspF?
009100120423     d IndDspF         ds                  inz
009200120423         // -?Abilitazione tasti funzionali?
009300120423         // -?Emissione messaggio di errore?
009400120423     d   ErrMessage                    n   overlay(IndDspF : 28)
009500120423         // -?Indicatori per Attibuti di visualizzazione?
009600120423         // -?Posizionamento cursore & segnalazione errore?
009700120423     d   PosCurVER                     n   overlay(IndDspF : 51)
009800120423         //   -?Riemissione videata?
009900120423     d   ErrGenerico                   n   overlay(IndDspF : 99)
010000120423
010100120423       // -?Parametri ricevuti & per batch?
010200120423     d KPJBA         e ds
010300120423     d trulCAPds       ds                  inz
010400120423     d   V1Cver
010500120423
010600120423       // -?Parametri per Reperimento dati utente?
010700120423     d TIBS34ds      e ds
010800120423
010900120423       //--------------------------------------------------------------
011000120423       //?Definizione variabili globali.                               ?
011100120423       //--------------------------------------------------------------
011200120423
011300120423       // -?Flags booleani?
011400120423     d $Fine           s               n   inz
011500120423     d $InzD01         s               n   inz(*on)
011600120423
011700120423       // -?Variabili per la gestione del video?
011800120423     d $Video          s              2    inz('D1')
011900120423
012000120423       //--------------------------------------------------------------
012100120423       //?Definizione prototipi procedure.                             ?
012200120423       //--------------------------------------------------------------
012300120423
012400120423       // -?Reperimento dati utente?
012500120423      /copy gaitrasrc/srcProtoPR,TIBS34R
012600120423
012700120423       // -?Sottomissione lavoro batch?
012800120423      /copy gaitrasrc/srcProtoPR,BCH10
012900120423
013000120423       // -?Richiamo diretto lavoro batch?
013100120423     d trulCAPc        pr                  extpgm('TRULCAPC')
013200120423     d   kpjba                             likeds(KPJBA)
013300120423
013400120423       // -?Parametri API QCAPCMD (Process Commands)?
013500120423     d Qcmd            s           2048    inz  varying
013600120423      /copy qSysInc/qRpgleSrc,QCAPCMD
013700120423       // -?API QCAPCMD (Process Commands)?
013800120423      /copy gaitrasrc/srcProtoPR,QCAPCMD
013900120423
014000120423       // -?Parametri gestione errori API.?
014100120423      /copy qsysinc/qrpglesrc,QUSEC
014200120423
014300120423       //--------------------------------------------------------------
014400120423       //?Definizione key-list.                                        ?
014500120423       //--------------------------------------------------------------
014600120423
014700120423
014800120423       //--------------------------------------------------------------
014900120423       //?M A I N - L I N E                                            ?
015000120423       //--------------------------------------------------------------
015100120423
015200120423     c     *Entry        plist
015300120423     c                   parm                    KPJBA
015400120423
015500120423      /free
015600120423
015700120423       // -?Operazioni iniziali?
015800120423       exsr  sr_RoutInz;
015900120423
016000120423       // -?Ciclo di gestione del file video?
016100120423       DOW  $Fine = *off;
016200120423         exsr  sr_GesD01;
016300120423       ENDDO;
016400120423
016500120423       // -?Operazioni finali?
016600120423       exsr  sr_RoutEnd;
016700120423
016800120423       //--------------------------------------------------------------
016900120423       //?Operazioni iniziali.                                         ?
017000120423       //--------------------------------------------------------------
017100120423       BEGSR  sr_RoutInz;
017200120423
017300120423         *inLR = *on;
017400120423
017500120423         // -?Reperimento dati job?
017600120423         exsr  sr_DatiJob;
017700120423
017800120423         // -?Impostazione nome programma a video?
017900120423         V1Tpgm = SDSpgm;
018000120423
018100120423       ENDSR;
018200120423
018300120423       //--------------------------------------------------------------
018400120423       //?Reperimento Dati del job (Utente/Operativi).                 ?
018500120423       //--------------------------------------------------------------
018600120423       BEGSR  sr_DatiJob;
018700120423
018800120423         in(E) §AzUte;
018900120423         if NOT %error;
019000120423           in(E) §DatiUte;
019100120423         endif;
019200120423         if %error or RSut = *blanks;
019300120423           clear TIBS34ds;
019400120423           tibs34r ( tibs34ds );
019500120423           in §AzUte;
019600120423           in §DatiUte;
019700120423         endif;
019800120423
019900120423       ENDSR;
020000120423
020100120423       //--------------------------------------------------------------
020200120423       //?Gestione videata D01.                                        ?
020300120423       //--------------------------------------------------------------
020400120423       BEGSR  sr_GesD01;
020500120423
020600120423         // -?Inizializzazione videata?
020700120423         if  $InzD01 = *on;
020800120423           clear  ulCapD1;
020900120423           $InzD01 = *off;
021000120423           // -?Ctrl Sistema Informativo per verificare se Sede o Filiale?
021100120423           if  %subst(KNSIF:1:3) = 'FIL';
021200120423             ErrGenerico = *on;
021300120423             ErrMessage  = *on;
021400120423             V1Dmsg = $Msg(01);
021500120423           endif;
021600120423         endif;
021700120423
021800120423         // -?Emissione videata?
021900120423         write  ULCAPT1;
022000120423         exfmt  ULCAPD1;
022100120423
022200120423         clear  V1Dmsg;
022300120423         reset  ErrMessage;
022400120423         reset  ErrGenerico;
022500120423
022600120423         SELECT;
022700120423
022800120423           // -?Sistema Informativo errato => FINE?
022900120423           WHEN  %subst( KNSIF : 1 : 3 ) = 'FIL';
023000120423             $Fine = *on;
023100120423
023200120423           // -?F3=Fine?
023300120423           WHEN  dsp_aid = c_F03;
023400120423             $Fine = *on;
023500120423
023600120423           // -?F9=Visualizzazione WrkF WFCAP00F?
023700120423           WHEN  dsp_aid = c_F09;
023800120423             Qcmd = 'DSPDBF file(*libl/WFCAP00F) mbr(*first) output(*)';
023900120423             exsr  sr_ExecCmd;
024000120423
024100120423           // -?Invio / F6=Conferma?
024200120423           OTHER;
024300120423             exsr  sr_CtrD01;
024400120423             select;
024500120423               when  ErrGenerico = *on;
024600120423                 leavesr;
024700120423               when  dsp_aid = c_F06;
024800120423                 exsr  sr_F06D01;
024900120423                 $Fine = *on;
025000120423             endsl;
025100120423
025200120423         ENDSL;
025300120423
025400120423       ENDSR;
025500120423
025600120423       //--------------------------------------------------------------
025700120423       //?Controllo dati immessi nella videata D01.                    ?
025800120423       //--------------------------------------------------------------
025900120423       BEGSR  sr_CtrD01;
026000120423
026100120423         // -?Spegnimento indicatori di posizionamento cursore?
026200120423         %subst(IndDspF : 28) = *off;
026300120423
026400120423         Select;
026500120423
026600120423           // -?Controllo immissione codice completo?
026700120423           When  V1Cver <= *zero;
026800120423             ErrMessage  = *on;
026900120423             ErrGenerico = *on;
027000120423             PosCurVER   = *on;
027100120423             V1Dmsg = $Msg(02);
027200120423             leavesr;
027300120423
027400120423         EndSl;
027500120423
027600120423       ENDSR;
027700120423
027800120423       //--------------------------------------------------------------
027900120423       //?Richiamo CL TRULCAPC.                                        ?
028000120423       //--------------------------------------------------------------
028100120423       BEGSR  sr_F06D01;
028200120423
028300120423         // -?Impostazione parametri?
028400120423         kpjbu = TrulCAPds;
028500120423
028600120423         // -?Richiamo interattivo?
028700120423         trulCAPc ( kpjba );
028800120423
028900120423       ENDSR;
029000120423
029100120423       //--------------------------------------------------------------
029200120423       //?Esecuzione del comando (già impostato).                      ?
029300120423       //--------------------------------------------------------------
029400120423       BEGSR  sr_ExecCmd;
029500120423
029600120423         clear Qcap0100;
029700120423         Qcabcsdh = *off;
029800120423         Qcapa    = *off;
029900120423         Qcacmdss = *off;
030000120423         Qcaerved = *allX'00';
030100120423
030200120423         clear Qusec;
030300120423         Qusbprv  = %size(Qusec);
030400120423
030500120423         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
030600120423                           %size(Qcap0100) : 'CPOP0100' : *omit :
030700120423                           0 : 0 : Qusec);
030800120423
030900120423         //if  Qusei <> *blank;
031000120423         //  ...;
031100120423         //endif;
031200120423
031300120423       ENDSR;
031400120423
031500120423       //--------------------------------------------------------------
031600120423       //?Operazioni finali.                                           ?
031700120423       //--------------------------------------------------------------
031800120423       BEGSR  sr_RoutEnd;
031900120423
032000120423         // -?Chiusura pgm?
032100120423         return;
032200120423
032300120423       ENDSR;
032400120423
032500120423      /end-free
032600120423
032700120423       //--------------------------------------------------------------
032800120423       //?Schiere a tempo di compilazione.
032900120423       //--------------------------------------------------------------
033000120423
033100120423** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
033200120423Funzione eseguibile solo in ambiente di SEDE: premere "Invio" per uscire       1
033300120423Versione Cappario obbligatoria                                                 2
