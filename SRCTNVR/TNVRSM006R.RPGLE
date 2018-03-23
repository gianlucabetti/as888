000100091009       //--------------------------------------------------------------
000200091009       //?SNDBRKMSG durante ciclo di attesa...                         ?
000300091009       //--------------------------------------------------------------
000400091009
000500091009     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600091009     h dftactgrp(*no) actgrp('QILE')
000700091009
000800091009       //--------------------------------------------------------------
000900091009       //?Dichiarazione file.                                          ?
001000091009       //--------------------------------------------------------------
001100091009
001200091009       // - Video
001300091009     fTNVRSM006Dcf   e             workstn
001400091009     f                                     indds(IndDspF)
001500091009     f                                     infds(InfDspF)
001600091009
001700091009       //--------------------------------------------------------------
001800091009       //?Definizione costanti.                                        ?
001900091009       //--------------------------------------------------------------
002000091009
002100091009       // - Tasti funzionali a video
002200091009     d c_F01           c                   const(x'31')
002300091009     d c_F02           c                   const(x'32')
002400091009     d c_F03           c                   const(x'33')
002500091009     d c_F05           c                   const(x'35')
002600091009     d c_F06           c                   const(x'36')
002700091009     d c_F07           c                   const(x'37')
002800091009     d c_F08           c                   const(x'38')
002900091009     d c_F09           c                   const(x'39')
003000091009     d c_F10           c                   const(x'3A')
003100091009     d c_F12           c                   const(x'3C')
003200091009     d c_F13           c                   const(x'B1')
003300091009     d c_F14           c                   const(x'B2')
003400091009     d c_F15           c                   const(x'B3')
003500091009     d c_F16           c                   const(x'B4')
003600091009     d c_F17           c                   const(x'B5')
003700091009     d c_F18           c                   const(x'B6')
003800091009     d c_F19           c                   const(x'B7')
003900091009     d c_F20           c                   const(x'B8')
004000091009     d c_F21           c                   const(x'B9')
004100091009     d c_F22           c                   const(x'BA')
004200091009     d c_F23           c                   const(x'BB')
004300091009     d c_F24           c                   const(x'BC')
004400091009     d c_Enter         c                   const(x'F1')
004500091009     d c_RollDown      c                   const(x'F4')
004600091009     d c_RollUp        c                   const(x'F5')
004700091009
004800091009       //--------------------------------------------------------------
004900091009       //?Definizione schiere.                                         ?
005000091009       //--------------------------------------------------------------
005100091009
005200091009
005300091009       //--------------------------------------------------------------
005400091009       //?Definizione aree dati.                                       ?
005500091009       //--------------------------------------------------------------
005600091009
005700091009       // - Dati utente
005800091009     d §AzUte        e ds                  extname(AZUTE00F)
005900091009     d                                     dtaara
006000091009     d §DatiUte      e ds                  extname(dDatiUte)
006100091009     d                                     dtaara
006200091009
006300091009       //--------------------------------------------------------------
006400091009       //?Definizione strutture dati.                                  ?
006500091009       //--------------------------------------------------------------
006600091009
006700091009       // - Status ds
006800091009     d Status         sds
006900091009     d  SDSpgm           *proc
007000091009     d  SDSprm           *parms
007100091009     d  SDSdta               191    198
007200091009     d  SDSjob               244    253
007300091009     d  SDSusr               254    263
007400091009
007500091009       // - InfDS
007600091009     d InfDspF         ds
007700091009     d   dsp_aid             369    369a
007800091009
007900091009       // - Indicatori su DspF
008000091009     d IndDspF         ds                  inz
008100091009       //   · Emissione messaggio di errore
008200091009     d   ErrMessage                    n   overlay(IndDspF : 28)
008300091009       //   · Testo obbligatorio
008400091009     d   PosCurLOOP                    n   overlay(IndDspF : 51)
008500091009     d   PosCurTEXT                    n   overlay(IndDspF : 52)
008600091009     d   PosCurNLM1                    n   overlay(IndDspF : 53)
008700091009     d   PosCurNLM2                    n   overlay(IndDspF : 54)
008800091009     d   PosCurNLM3                    n   overlay(IndDspF : 55)
008900091009     d   PosCurNLM4                    n   overlay(IndDspF : 56)
009000091009     d   PosCurNLM5                    n   overlay(IndDspF : 57)
009100091009       //   · Riemissione videata
009200091009     d   ErrGenerico                   n   overlay(IndDspF : 99)
009300091009
009400091009       // - Schiera di n° cicli ai quale inviare il *msg
009500091009     d ds_loop         ds            25    inz
009600091009     d   V1nlm1                       5s 0 inz
009700091009     d   V1nlm2                       5s 0 inz
009800091009     d   V1nlm3                       5s 0 inz
009900091009     d   V1nlm4                       5s 0 inz
010000091009     d   V1nlm5                       5s 0 inz
010100091009     d     $NLM                1     25  0 inz  dim(5)
010200091009
010300091009       // - Parametri ricevuti
010400091009     d KPJBA         e ds
010500091009
010600091009       // - Parametri per Reperimento dati utente
010700091009     d TIBS34ds      e ds                  inz
010800091009
010900091009       //--------------------------------------------------------------
011000091009       //?Definizione variabili globali.                               ?
011100091009       //--------------------------------------------------------------
011200091009
011300091009       // - Flags booleani
011400091009     d $Fine           s               n   inz
011500091009     d $Err            s               n   inz
011600091009     d $InzD01         s               n   inz(*on)
011700091009
011800091009       // - Indici di schiera
011900091009     d xx              s              5  0 inz
012000091009     d yy              s              3  0 inz
012100091009
012200091009       // - Campi per QCMDEXC
012300091009     d Qcmd            s            128    inz  varying
012400091009
012500091009       //--------------------------------------------------------------
012600091009       //?Definizione prototipi procedure.                             ?
012700091009       //--------------------------------------------------------------
012800091009
012900091009       // - Execuzione comando di sistema
013000091009      /copy gaitrasrc/srcProtoPR,QCMDEXC
013100091009
013200091009       // - Reperimento dati utente
013300091009      /copy gaitrasrc/srcProtoPR,TIBS34R
013400091009
013500091009       //--------------------------------------------------------------
013600091009       //?Definizione key-list.                                        ?
013700091009       //--------------------------------------------------------------
013800091009
013900091009
014000091009       //--------------------------------------------------------------
014100091009       //?M A I N - L I N E                                            ?
014200091009       //--------------------------------------------------------------
014300091009
014400091009     c     *Entry        plist
014500091009     c                   parm                    KPJBA
014600091009
014700091009      /free
014800091009
014900091009       // -?Operazioni iniziali?
015000091009       exsr  sr_RoutInz;
015100091009
015200091009       // -?Gestione video?
015300091009       DoW  Not  $Fine;
015400091009         exsr  sr_GesD01;
015500091009       EndDo;
015600091009
015700091009       // -?Operazioni finali?
015800091009       exsr sr_RoutEnd;
015900091009
016000091009       //--------------------------------------------------------------
016100091009       //?Operazioni iniziali.                                         ?
016200091009       //--------------------------------------------------------------
016300091009       BEGSR  sr_RoutInz;
016400091009
016500091009         *inLR = *on;
016600091009
016700091009         // -?Reperimento dati job?
016800091009         exsr  sr_DatiJob;
016900091009
017000091009       ENDSR;
017100091009
017200091009       //--------------------------------------------------------------
017300091009       //?Reperimento Dati del job (Utente/Operativi).                 ?
017400091009       //--------------------------------------------------------------
017500091009       BEGSR  sr_DatiJob;
017600091009
017700091009         in(E) §AzUte;
017800091009         if NOT %error;
017900091009           in(E) §DatiUte;
018000091009         endif;
018100091009         if %error or RSut = *blanks;
018200091009           clear TIBS34ds;
018300091009           tibs34r ( tibs34ds );
018400091009           in §AzUte;
018500091009           in §DatiUte;
018600091009         endif;
018700091009
018800091009       ENDSR;
018900091009
019000091009       //--------------------------------------------------------------
019100091009       //?Gestione filtro di lancio.                                   ?
019200091009       //--------------------------------------------------------------
019300091009       BEGSR  sr_GesD01;
019400091009
019500091009         // - Inizializzazione videata
019600091009         if  $InzD01 = *on;
019700091009           V1Tpgm = SDSpgm;
019800091009           V1loop = 1500;
019900091209           V1text = 'Test invio *msg durante loop...';
020000091009           V1nlm1 = 1000;
020100091009           V1nlm2 = 1200;
020200091009           $InzD01 = *off;
020300091009         endif;
020400091009
020500091009         // - Emissione videata
020600091009         write  TNVRSM6W;
020700091009         write  TNVRSM6T;
020800091009         exfmt  TNVRSM6D;
020900091009
021000091009         clear  ErrMessage;
021100091009         clear  ErrGenerico;
021200091009         clear  V1Dmsg;
021300091009
021400091009         select;
021500091009
021600091009           // - F3 = Fine
021700091009           when  dsp_aid = c_F03;
021800091009             $Fine = *on;
021900091009
022000091009           other;
022100091009             exsr  sr_CtrD01;
022200091009             select;
022300091009               // - Errore
022400091009               when  ErrGenerico;
022500091009                 leavesr;
022600091009               // - F6 = Conferma
022700091009               when  dsp_aid = c_F06;
022800091009                 exsr  sr_F06D01;
022900091009                 $Fine  = *on;
023000091009             endsl;
023100091009
023200091009         endsl;
023300091009
023400091009       ENDSR;
023500091009
023600091009       //--------------------------------------------------------------
023700091009       //?Controllo parametri di lancio                                ?
023800091009       //--------------------------------------------------------------
023900091009       BEGSR  sr_CtrD01;
024000091009
024100091009         %subst(IndDspF : 50) = *off;
024200091009
024300091009         // - Numero di "loop"
024400091009         select;
024500091009           // - obbligatorio
024600091009           when  V1loop = *zero;
024700091009             ErrMessage  = *on;
024800091009             ErrGenerico = *on;
024900091009             PosCurLOOP  = *on;
025000091009             V1Dmsg = 'Numero di "loop" obbligatorio.';
025100091009             leavesr;
025200091009           // - negativo
025300091009           when  V1loop < *zero;
025400091009             ErrMessage  = *on;
025500091009             ErrGenerico = *on;
025600091009             PosCurLOOP  = *on;
025700091009             V1Dmsg = 'Numero di "loop" negativo NON ammesso.';
025800091009             leavesr;
025900091009         endsl;
026000091009
026100091009         // - Messaggio obbligatorio
026200091009         if  V1text = *blank;
026300091009           ErrMessage  = *on;
026400091009           ErrGenerico = *on;
026500091009           PosCurTEXT  = *on;
026600091009           V1Dmsg = 'Testo del messaggio obbligatorio.';
026700091009           leavesr;
026800091009         endif;
026900091009
027000091009         // - Numero di "loop" ai quale inviare il *msg
027100091009         For  yy = 1  To  %elem( $NLM );
027200091009           select;
027300091009             // - negativo
027400091009             when  $NLM(yy) < *zero;
027500091009               ErrMessage  = *on;
027600091009               ErrGenerico = *on;
027700091009               V1Dmsg = 'Num. "loop" negativo NON ammesso.';
027800091009             // - oltre il massimo ammesso
027900091009             when  $NLM(yy) > V1loop;
028000091009               ErrMessage  = *on;
028100091009               ErrGenerico = *on;
028200091009               V1Dmsg = 'Num. "loop" oltre il massimo previsto.';
028300091009           endsl;
028400091009           select;
028500091009             when  Not ErrMessage;
028600091009             when  yy = 1;
028700091009               PosCurNLM1 = *on;
028800091009               leavesr;
028900091009             when  yy = 2;
029000091009               PosCurNLM2 = *on;
029100091009               leavesr;
029200091009             when  yy = 3;
029300091009               PosCurNLM3 = *on;
029400091009               leavesr;
029500091009             when  yy = 4;
029600091009               PosCurNLM4 = *on;
029700091009               leavesr;
029800091009             when  yy = 5;
029900091009               PosCurNLM5 = *on;
030000091009               leavesr;
030100091009           endsl;
030200091009         endfor;
030300091009
030400091009       ENDSR;
030500091009
030600091009       //--------------------------------------------------------------
030700091009       //?Gestione tasto funzionale F6=Conferma                        ?
030800091009       //--------------------------------------------------------------
030900091009       BEGSR  sr_F06D01;
031000091009
031100091009         // - Ciclo di giri indicato
031200091009         For  xx = 1  To V1loop;
031300091009
031400091009           // - Attesa di 1 secondo in ogni giro
031500091009           Qcmd = 'DLYJOB dly(1)';
031600091009           ExecuteCommand ( Qcmd : %len(Qcmd) );
031700091009
031800091009           // - Messaggio ai n° giri indicati
031900091009           clear  Qcmd;
032000091009           select;
032100091009             when  xx = V1nlm1;
032200091009               Qcmd = 'SNDBRKMSG msg(''1° - ';
032300091009             when  xx = V1nlm2;
032400091009               Qcmd = 'SNDBRKMSG msg(''2° - ';
032500091009             when  xx = V1nlm3;
032600091009               Qcmd = 'SNDBRKMSG msg(''3° - ';
032700091009             when  xx = V1nlm4;
032800091009               Qcmd = 'SNDBRKMSG msg(''4° - ';
032900091009             when  xx = V1nlm5;
033000091009               Qcmd = 'SNDBRKMSG msg(''5° - ';
033100091009           endsl;
033200091009           if  Qcmd <> *blank;
033300091009             Qcmd += %trim( V1text ) + ''') +
033400091009                     tomsgq(' + SDSjob + ') +
033500091009                     msgtype(*info) +
033600091009                     ';
033700091009             ExecuteCommand ( Qcmd : %len(Qcmd) );
033800091009           endif;
033900091009
034000091009         EndFor;
034100091009
034200091009       ENDSR;
034300091009
034400091009       //--------------------------------------------------------------
034500091009       //?Stampa segnalazione dell'errore rilevato via SQL             ?
034600091009       //--------------------------------------------------------------
034700091009       BEGSR  sr_PrintErrSQL;
034800091009
034900091009         //?Stampa del Dump?
035000091009         Dump(A);
035100091009
035200091009         //?Stampa del Job-Log?
035300091009         Qcmd = 'DSPJOBLOG job(*) output(*print)';
035400091009         ExecuteCommand ( Qcmd : %len(Qcmd) );
035500091009
035600091009         //?Chiusura del programma?
035700091009         exsr  sr_RoutEnd;
035800091009
035900091009       ENDSR;
036000091009
036100091009       //--------------------------------------------------------------
036200091009       //?Operazioni finali.                                           ?
036300091009       //--------------------------------------------------------------
036400091009       BEGSR  sr_RoutEnd;
036500091009
036600091009         //?"Chiusura" pgm?
036700091009         return;
036800091009
036900091009       ENDSR;
037000091009
037100091009      /end-free
