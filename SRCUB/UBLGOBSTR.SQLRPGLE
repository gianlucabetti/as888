000100111128       //==============================================================
000200170302       // UBLGOBSTR - Finestra di storicizzazione
000300120405       //==============================================================
000400111128
000500111128       //--------------------------------------------------------------
000600120207       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700111128       //--------------------------------------------------------------
000800111128
000900111128     /*PRM  dbgview(*source)
001000111128     /*END
001100111128
001200111128       //--------------------------------------------------------------
001300120207       // Specifiche di controllo.                                     ?
001400111128       //--------------------------------------------------------------
001500111128
001600111128     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700120510     h dftactgrp(*no) bnddir('UBBNDDIR')
001800111128     h alwnull(*inputonly)
001900111128
002000111128       //--------------------------------------------------------------
002100120207       // Dichiarazione file.                                          ?
002200111128       //--------------------------------------------------------------
002300120215       // - Video
002400170302     fUBLGOBSTD cf   e             workstn
002500170308     f                                     indds(IndDspF)
002600120207     f                                     infds(InfDspF)
002700111128
002800111128
002900111128       //--------------------------------------------------------------
003000120207       // Definizione costanti.                                        ?
003100111128       //--------------------------------------------------------------
003200120207
003300120207       // - Tasti funzionali a video
003400120207     d c_F01           c                   const(x'31')
003500120207     d c_F02           c                   const(x'32')
003600120207     d c_F03           c                   const(x'33')
003700120207     d c_F05           c                   const(x'35')
003800120207     d c_F06           c                   const(x'36')
003900120207     d c_F07           c                   const(x'37')
004000120207     d c_F08           c                   const(x'38')
004100120207     d c_F09           c                   const(x'39')
004200120207     d c_F10           c                   const(x'3A')
004300120207     d c_F12           c                   const(x'3C')
004400120207     d c_F13           c                   const(x'B1')
004500120207     d c_F14           c                   const(x'B2')
004600120207     d c_F15           c                   const(x'B3')
004700120207     d c_F16           c                   const(x'B4')
004800120207     d c_F17           c                   const(x'B5')
004900120207     d c_F18           c                   const(x'B6')
005000120207     d c_F19           c                   const(x'B7')
005100120207     d c_F20           c                   const(x'B8')
005200120207     d c_F21           c                   const(x'B9')
005300120207     d c_F22           c                   const(x'BA')
005400120207     d c_F23           c                   const(x'BB')
005500120207     d c_F24           c                   const(x'BC')
005600120207     d c_Enter         c                   const(x'F1')
005700120207     d c_RollDown      c                   const(x'F4')
005800120207     d c_RollUp        c                   const(x'F5')
005900111128
006000111128
006100111128       //--------------------------------------------------------------
006200120207       // Definizione schiere.                                         ?
006300111128       //--------------------------------------------------------------
006400111128
006500111128
006600111128       //--------------------------------------------------------------
006700120207       // Definizione aree dati.                                       ?
006800111128       //--------------------------------------------------------------
006900111128
007000111128
007100111128       //--------------------------------------------------------------
007200120207       // Definizione strutture dati.                                  ?
007300111128       //--------------------------------------------------------------
007400120510
007500120510
007600111128       //--------------------------------------------------------------
007700120207       // Definizione variabili globali.                               ?
007800111128       //--------------------------------------------------------------
007900111128
008000120207       // - InfDS
008100120207     d InfDspF         ds
008200120207     d   dsp_aid             369    369a                                        AID byte
008300120207
008400120207       // - Indicatori su DspF
008500170308     d IndDspF         ds
008600120207        // - Indicatori di errore
008700170308     d  F06HI                         1n   overlay(IndDspF : 01)
008800170308     d  ErrMsg                        1n   overlay(IndDspF : 28)
008900120620       //d  ErrGenerico                   1n   overlay(IndDspF : 99)
009000170308       //d  ErrMsg         s              1n
009100120620     d  ErrGenerico    s              1n
009200120404
009300120207       // - Flags booleani
009400120207     d $Fine           s               n   inz(*off)
009500120510     d $InzW01         s               n   inz(*on)
009600120208     d ErrBlock        s               n   inz(*off)
009700120207
009800120207       // - Campi associati al video
009900120510     d $Video          s              2    inz('W1')
010000120207
010100120207       // - Variabili di appoggio
010200120510     d wData08         s              8s 0
010300120405     d wX              s              5s 0
010400170307     d wSQL            s           2048    inz  varying
010500170308     d wLibP           s             10a   inz
010600170308     d wFilP           s             10a   inz
010700170308     d wMbrP           s             10a   inz
010800170308     d wLibA           s             10a   inz
010900170308     d wFilA           s             10a   inz
011000170308     d wMbrA           s             10a   inz
011100170331     d wNota           s               n   inz
011200111128
011300111128       //--------------------------------------------------------------
011400120210       // Definizione procedure usate
011500111128       //--------------------------------------------------------------
011600120207
011700120210       // - Parametri gestione errori API
011800120207      /copy qSysInc/qRpgleSrc,QUSEC
011900170303
012000170303       // Reperimento NETA sistema AS/400 corrente?
012100170303     d currSysNeta     s              8a   inz
012200170303      /copy gaitrasrc/srcProtoPr,UBRTVNETA
012300170303
012400170303       // - Parametri API QCAPCMD (Process Commands)
012500170303     d Qcmd            s           2048    inz   varying
012600170303      /copy qSysInc/qRpgleSrc,QCAPCMD
012700170303      /copy gaitrasrc/srcProtoPR,QCAPCMD
012800120404
012900170302     d UBLGOBSTR       pr
013000170307     d  DtEstr                         d
013100170307     d  Logica                        2
013200170307     d  Obj                          10
013300170302     d  Lib                          10
013400170302     d  ObjT                          7
013500170303     d  Dati                        130
013600170302     d  Exfmt                         1
013700120404
013800170302     d UBLGOBSTR       pi
013900170307     d  DtEstr                         d
014000170307     d  Logica                        2
014100170302     d  Obj                          10
014200170302     d  Lib                          10
014300170302     d  ObjT                          7
014400170303     d  Dati                        130
014500170302     d  Exfmt                         1
014600111128
014700111128       //--------------------------------------------------------------
014800120208       // Definizione key-list.                                        ?
014900111128       //--------------------------------------------------------------
015000111128
015100111128
015200111128       //--------------------------------------------------------------
015300120208       // M A I N - L I N E                                            ?
015400111128       //--------------------------------------------------------------
015500111128
015600111128      /free
015700111128
015800120208       // - Operazioni iniziali
015900111128       exsr  sr_RoutInz;
016000111128
016100120207       // - Gestione video
016200120207       DOW  $Fine = *off;
016300120207         select;
016400120510           when  $Video = 'W1';
016500120510             exsr  sr_GesW01;
016600120207           other;
016700120207             $Fine = *on;
016800120207         endsl;
016900120207       ENDDO;
017000120207
017100120208       // - Operazioni finali
017200111128       exsr  sr_RoutEnd;
017300111128
017400111128       //--------------------------------------------------------------
017500120208       // Operazioni iniziali
017600111128       //--------------------------------------------------------------
017700111128       BEGSR sr_RoutInz;
017800111128
017900120620         // Nel caso che, tra la passata chiamata a questo pgm e questa,
018000120620         // ci sia stata una emissione a video del pgm chiamante
018100120620         // chiudo e riapro il DSPF contenente la finestra
018200120620         // per evitare il permanere del primo sfondo sotto la finestra.
018300120620         if exfmt = *on;
018400170302           close UBLGOBSTD;
018500170302           open  UBLGOBSTD;
018600120620         endif;
018700120620
018800120620         *inRT = *on;
018900120620         $Fine = *off;
019000120620         $InzW01 = *on;
019100120620         //IndDspF  = *off;
019200111128
019300120208         // - Impostazione opzioni per SQL
019400111128         exec SQL   set option   DynUsrPrf = *owner,
019500111128                                 CloSqlCsr = *endmod;
019600170303
019700170303         // Verifica del sistema AS/400 corrente
019800170303         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
019900170303           exsr sr_RoutEnd;
020000170303         endif;
020100111128
020200111128       ENDSR;
020300120207
020400120207       //--------------------------------------------------------------
020500120510       // Gestione videata W01
020600120207       //--------------------------------------------------------------
020700120510       BEGSR  sr_GesW01;
020800120207
020900120207         // - Inizializzazione videata
021000120510         if  $InzW01   = *on;
021100120510           exsr  sr_InzW01;
021200120510           $InzW01  = *off;
021300120207         endif;
021400120207
021500120207         // - Emissione videata
021600170302         exfmt  GOBSW01;
021700120207
021800120208         reset  ErrMsg;
021900120207
022000120207         SELECT;
022100120207
022200120207           // - F3=Fine
022300120207           WHEN  dsp_aid = c_F03;
022400120510             exsr sr_F03W01;
022500120207
022600120207           // - F12=Ritorno
022700120207           WHEN  dsp_aid = c_F12;
022800120510             exsr sr_F12W01;
022900120207
023000120207           // - Invio / F6=Conferma
023100120207           OTHER;
023200120510             exsr  sr_CtrW01;
023300120207             if  ErrGenerico = *on;
023400120207               leavesr;
023500120207             endif;
023600120207             if  dsp_aid = c_F06;
023700120510               exsr sr_F06W01;
023800120207             endif;
023900120207
024000120207         ENDSL;
024100120207
024200120207       ENDSR;
024300120207
024400120207       //--------------------------------------------------------------
024500120510       // Inizializzazione videata W01
024600120207       //--------------------------------------------------------------
024700120510       BEGSR  sr_InzW01;
024800120207
024900120207         // - Pulizia videata
025000170302         clear  GOBSW01;
025100120620
025200120620         // - Imposto i campi video con i dati in ingresso
025300170307         w01DtEstr= %dec(DtEstr:*eur);
025400170307         w01Logica= Logica ;
025500170307         w01Obj   = Obj  ;
025600170302         w01Lib   = Lib  ;
025700170302         w01ObjT  = ObjT ;
025800170302
025900170302         // - Imposto i campi video con i dati di default
026000170303         w01LibSP = %subst(Dati:46:10);
026100170303         w01FilSP = %subst(Dati:36:10);
026200170303         w01MbrSP = %subst(Dati:56:10);
026300170302         w01LibSA = 'SRCTRAOLD';
026400170302         w01FilSA = 'SRCOLD' + %char(%subdt(%date():*Y));
026500170303         w01MbrSA = %subst(Dati:56:10);
026600170303         w01LibNP = %subst(Dati:46:10);
026700170303         w01FilNP = 'SRCNOTE';
026800170303         w01MbrNP = %subst(Dati:56:10);
026900170302         w01LibNA = 'SRCTRAOLD';
027000170302         w01FilNA = 'NOTOLD' + %char(%subdt(%date():*Y));
027100170303         w01MbrNA = %subst(Dati:56:10);
027200120207
027300120207       ENDSR;
027400120207
027500120207       //--------------------------------------------------------------
027600120510       // Gestione tasto funzionale F3 da videata W01                  ?
027700120510       // F3=Fine                                                      ?
027800120207       //--------------------------------------------------------------
027900120510       BEGSR  sr_F03W01;
028000120207
028100120207         // - Chiusura del programma
028200120207         $Fine = *on;
028300120207
028400120207       ENDSR;
028500120207
028600120207       //--------------------------------------------------------------
028700120510       // Gestione tasto funzionale F6 da videata W01
028800120208       // F6=Conferma
028900120207       //--------------------------------------------------------------
029000120510       BEGSR  sr_F06W01;
029100120405
029200170306         // doppia conferma
029300170308         if F06HI = *off;
029400170308           eval F06HI = *on;
029500170306           leavesr;
029600170306         else;
029700170308           eval F06HI = *off;
029800170306         endif;
029900170308
030000170306         // cancello oggetto
030100170306         exsr sr_dltobj;
030200170306
030300170306         // cancello rcd WAFOB
030400170306         exsr sr_dltrcdFOB;
030500170306
030600170306         if W01MbrSP <> *blank;
030700170308           wLibP  = W01LibSP;
030800170308           wFilP  = W01FilSP;
030900170308           wMbrP  = W01MbrSP;
031000170308           wLibA  = W01LibSA;
031100170308           wFilA  = W01FilSA;
031200170308           wMbrA  = W01MbrSA;
031300170306           // copio sorgente
031400170331           wNota = *off;
031500170308           exsr sr_cpysrcmbr;
031600170306           // cancello sorgente
031700170331           wNota = *off;
031800170308           exsr sr_rmvsrcmbr;
031900170308           // cancello sorgente £
032000170308           wMbrP  = %trim(W01MbrSP) + '£';
032100170308           exsr sr_rmvsrcmbr;
032200170308
032300170308           wLibP  = W01LibNP;
032400170308           wFilP  = W01FilNP;
032500170308           wMbrP  = W01MbrNP;
032600170308           wLibA  = W01LibNA;
032700170308           wFilA  = W01FilNA;
032800170308           wMbrA  = W01MbrNA;
032900170306           // copio nota
033000170331           wNota = *on;
033100170308           exsr sr_cpysrcmbr;
033200170306           // cancello nota
033300170331           wNota = *on;
033400170308           exsr sr_rmvsrcmbr;
033500170306         endif;
033600120215
033700120510         // Esco dal pgm (solo se non ho errori da mostrare)
033800120208         if ErrMsg = *off;
033900120208           $Fine = *on;
034000170308         else;
034100170331           VIDmsg = 'Almeno un errore durante la storicizzazione: +
034200170331                    controllare manualmente i vari passaggi +
034300170331                    (le note non sono monitorate).';
034400120208         endif;
034500120207
034600120207       ENDSR;
034700120208
034800120207       //--------------------------------------------------------------
034900120510       // Gestione tasto funzionale F12 da videata W01
035000120208       // F12=Ritorno
035100120207       //--------------------------------------------------------------
035200120510       BEGSR  sr_F12W01;
035300120207
035400120207         // - Chiusura del programma
035500120207         $Fine = *on;
035600120207
035700120207       ENDSR;
035800120207
035900120207       //--------------------------------------------------------------
036000120510       // Controllo videata W01                                        ?
036100120207       //--------------------------------------------------------------
036200120510       BEGSR  sr_CtrW01;
036300120207
036400120620         //IndDspF  = *off;
036500120208
036600120207
036700120207       ENDSR;
036800170303
036900170303       //--------------------------------------------------------------
037000170303       // Cancella oggetto
037100170303       //--------------------------------------------------------------
037200170303       BEGSR  sr_DltObj;
037300170303
037400170303         QCmd =
037500170303          'DLTOBJ OBJ(' + %trim(W01Lib) ;
037600170303         QCmd = %trim(QCmd) + '/' + %trim(W01Obj);
037700170303         QCmd = %trim(QCmd) + ') OBJTYPE(' + %trim(W01ObjT);
037800170303         QCmd = %trim(QCmd) + ')';
037900170303
038000170303         select;
038100170303          // se sono su SETRAS
038200170303          when  %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
038300170303           // cancello oggetto sul sistema
038400170303           exsr sr_execCmd;
038500170308           if  Qusei <> *blank;
038600170308             ErrMsg = *on;
038700170308           endif;
038800170308
038900170303           // cancello oggetto su AS888
039000170308           QCmd = 'RUNRMTCMD CMD(''' + QCmd + ''') RMTLOCNAME(AS888)';
039100170303           exsr sr_execCmd;
039200170308           if  Qusei <> *blank;
039300170308             ErrMsg = *on;
039400170308           endif;
039500170303
039600170303          // se sono su AS888
039700170303          when  %subst ( currSysNeta : 1 : 6 ) = 'AS888 ';
039800170303           // cancello oggetto sul sistema
039900170303           exsr sr_execCmd;
040000170308           if  Qusei <> *blank;
040100170308             ErrMsg = *on;
040200170308           endif;
040300170303
040400170303         endsl;
040500170303
040600170303       ENDSR;
040700170306
040800170306       //--------------------------------------------------------------
040900170306       // Cancella record WAFOB00F
041000170306       //--------------------------------------------------------------
041100170306       BEGSR  sr_DltRcdFOB;
041200170306
041300170307         //la cancellazione del rcd la eseguo solo sul sistema in cui sono
041400170307
041500170306         wSQL =
041600170306          'delete from WAFOB00F +
041700170306           where +
041800170307           FOBDTESTR = ''' + %char(DtEstr) + ''' and +
041900170307           FOBLOGICA = ''' + %trim(W01Logica) + ''' and +
042000170307           FOBOBJ    = ''' + %trim(W01Obj) + ''' and +
042100170307           FOBLIB    = ''' + %trim(W01Lib) + ''' and +
042200170307           FOBOBJT   = ''' + %trim(W01ObjT) + '''';
042300170306
042400170307         exec sql execute immediate :wSQL;
042500170308         if SQLCod < 0 or SQLCod = 100;
042600170308           ErrMsg = *on;
042700170308         endif;
042800170306
042900170306       ENDSR;
043000170308
043100170308       //--------------------------------------------------------------
043200170308       // Copia membro sorgente
043300170308       //--------------------------------------------------------------
043400170308       BEGSR  sr_CpySrcMbr;
043500170308
043600170308         QCmd =
043700170308          'CPYSRCF FROMFILE(' + %trim(wLibP) + '/' +%trim(wFilP) + ')';
043800170308          QCmd = %trim(QCmd) + ' TOFILE(' + %trim(wLibA) + '/' +
043900170308                 %trim(wFilA) + ')';
044000170308          QCmd = %trim(QCmd) + ' FROMMBR(' + %trim(wMbrP) + ')';
044100170308          QCmd = %trim(QCmd) + ' TOMBR(' + %trim(wMbrA) + ')';
044200170308
044300170308         select;
044400170308          // se sono su SETRAS, il sorgente ce l'ho su AS888
044500170308          when  %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
044600170308           QCmd = 'RUNRMTCMD CMD(''' + QCmd + ''') RMTLOCNAME(AS888)';
044700170308           exsr sr_execCmd;
044800170331           if  Qusei <> *blank
044900170331            and wNota = *off;
045000170308             ErrMsg = *on;
045100170308           endif;
045200170308
045300170308          // se sono su AS888, il sorgente ce l'ho qui
045400170308          when  %subst ( currSysNeta : 1 : 6 ) = 'AS888 ';
045500170308           // cancello oggetto sul sistema
045600170308           exsr sr_execCmd;
045700170331           if  Qusei <> *blank
045800170331            and wNota = *off;
045900170308             ErrMsg = *on;
046000170308           endif;
046100170308
046200170308         endsl;
046300170308
046400170308       ENDSR;
046500170308
046600170308       //--------------------------------------------------------------
046700170308       // Cancella sorgente origine
046800170308       //--------------------------------------------------------------
046900170308       BEGSR  sr_RmvSrcMbr;
047000170308
047100170308         QCmd =
047200170308          'RMVM FILE(' + %trim(wLibP) + '/' +%trim(wFilP) + ') +
047300170308          MBR(' + %trim(wMbrP) + ')';
047400170308
047500170308         select;
047600170308          // se sono su SETRAS, il sorgente ce l'ho su AS888
047700170308          when  %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
047800170308           QCmd = 'RUNRMTCMD CMD(''' + QCmd + ''') RMTLOCNAME(AS888)';
047900170308           exsr sr_execCmd;
048000170331           if  Qusei <> *blank
048100170331            and wNota = *off;
048200170308             ErrMsg = *on;
048300170308           endif;
048400170308
048500170308          // se sono su AS888, il sorgente ce l'ho qui
048600170308          when  %subst ( currSysNeta : 1 : 6 ) = 'AS888 ';
048700170308           // cancello oggetto sul sistema
048800170308           exsr sr_execCmd;
048900170331           if  Qusei <> *blank
049000170331            and wNota = *off;
049100170308             ErrMsg = *on;
049200170308           endif;
049300170308
049400170308         endsl;
049500170308
049600170308       ENDSR;
049700170303
049800170303       //--------------------------------------------------------------
049900170303       // Esecuzione del comando (già impostato).                      ?
050000170303       //--------------------------------------------------------------
050100170303       BEGSR  sr_ExecCmd;
050200170303
050300170303         clear Qcap0100;
050400170303         Qcabcsdh = *off;
050500170303         Qcapa    = *off;
050600170303         Qcacmdss = *off;
050700170303         Qcaerved = *allX'00';
050800170303
050900170303         clear Qusec;
051000170303         Qusbprv  = %size(Qusec);
051100170303
051200170303         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
051300170303                           %size(Qcap0100) : 'CPOP0100' : *omit :
051400170303                           0 : 0 : Qusec);
051500170303
051600170303       ENDSR;
051700111128
051800111128       //--------------------------------------------------------------
051900120208       // Operazioni finali
052000111128       //--------------------------------------------------------------
052100111128       BEGSR  sr_RoutEnd;
052200111128
052300120208         // - Uscita
052400111128         return;
052500111128
052600111128       ENDSR;
052700111128
052800111128      /end-free
