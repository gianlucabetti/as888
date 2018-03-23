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
011100111128
011200111128       //--------------------------------------------------------------
011300120210       // Definizione procedure usate
011400111128       //--------------------------------------------------------------
011500120207
011600120210       // - Parametri gestione errori API
011700120207      /copy qSysInc/qRpgleSrc,QUSEC
011800170303
011900170303       // Reperimento NETA sistema AS/400 corrente?
012000170303     d currSysNeta     s              8a   inz
012100170303      /copy gaitrasrc/srcProtoPr,UBRTVNETA
012200170303
012300170303       // - Parametri API QCAPCMD (Process Commands)
012400170303     d Qcmd            s           2048    inz   varying
012500170303      /copy qSysInc/qRpgleSrc,QCAPCMD
012600170303      /copy gaitrasrc/srcProtoPR,QCAPCMD
012700120404
012800170302     d UBLGOBSTR       pr
012900170307     d  DtEstr                         d
013000170307     d  Logica                        2
013100170307     d  Obj                          10
013200170302     d  Lib                          10
013300170302     d  ObjT                          7
013400170303     d  Dati                        130
013500170302     d  Exfmt                         1
013600120404
013700170302     d UBLGOBSTR       pi
013800170307     d  DtEstr                         d
013900170307     d  Logica                        2
014000170302     d  Obj                          10
014100170302     d  Lib                          10
014200170302     d  ObjT                          7
014300170303     d  Dati                        130
014400170302     d  Exfmt                         1
014500111128
014600111128       //--------------------------------------------------------------
014700120208       // Definizione key-list.                                        ?
014800111128       //--------------------------------------------------------------
014900111128
015000111128
015100111128       //--------------------------------------------------------------
015200120208       // M A I N - L I N E                                            ?
015300111128       //--------------------------------------------------------------
015400111128
015500111128      /free
015600111128
015700120208       // - Operazioni iniziali
015800111128       exsr  sr_RoutInz;
015900111128
016000120207       // - Gestione video
016100120207       DOW  $Fine = *off;
016200120207         select;
016300120510           when  $Video = 'W1';
016400120510             exsr  sr_GesW01;
016500120207           other;
016600120207             $Fine = *on;
016700120207         endsl;
016800120207       ENDDO;
016900120207
017000120208       // - Operazioni finali
017100111128       exsr  sr_RoutEnd;
017200111128
017300111128       //--------------------------------------------------------------
017400120208       // Operazioni iniziali
017500111128       //--------------------------------------------------------------
017600111128       BEGSR sr_RoutInz;
017700111128
017800120620         // Nel caso che, tra la passata chiamata a questo pgm e questa,
017900120620         // ci sia stata una emissione a video del pgm chiamante
018000120620         // chiudo e riapro il DSPF contenente la finestra
018100120620         // per evitare il permanere del primo sfondo sotto la finestra.
018200120620         if exfmt = *on;
018300170302           close UBLGOBSTD;
018400170302           open  UBLGOBSTD;
018500120620         endif;
018600120620
018700120620         *inRT = *on;
018800120620         $Fine = *off;
018900120620         $InzW01 = *on;
019000120620         //IndDspF  = *off;
019100111128
019200120208         // - Impostazione opzioni per SQL
019300111128         exec SQL   set option   DynUsrPrf = *owner,
019400111128                                 CloSqlCsr = *endmod;
019500170303
019600170303         // Verifica del sistema AS/400 corrente
019700170303         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
019800170303           exsr sr_RoutEnd;
019900170303         endif;
020000111128
020100111128       ENDSR;
020200120207
020300120207       //--------------------------------------------------------------
020400120510       // Gestione videata W01
020500120207       //--------------------------------------------------------------
020600120510       BEGSR  sr_GesW01;
020700120207
020800120207         // - Inizializzazione videata
020900120510         if  $InzW01   = *on;
021000120510           exsr  sr_InzW01;
021100120510           $InzW01  = *off;
021200120207         endif;
021300120207
021400120207         // - Emissione videata
021500170302         exfmt  GOBSW01;
021600120207
021700120208         reset  ErrMsg;
021800120207
021900120207         SELECT;
022000120207
022100120207           // - F3=Fine
022200120207           WHEN  dsp_aid = c_F03;
022300120510             exsr sr_F03W01;
022400120207
022500120207           // - F12=Ritorno
022600120207           WHEN  dsp_aid = c_F12;
022700120510             exsr sr_F12W01;
022800120207
022900120207           // - Invio / F6=Conferma
023000120207           OTHER;
023100120510             exsr  sr_CtrW01;
023200120207             if  ErrGenerico = *on;
023300120207               leavesr;
023400120207             endif;
023500120207             if  dsp_aid = c_F06;
023600120510               exsr sr_F06W01;
023700120207             endif;
023800120207
023900120207         ENDSL;
024000120207
024100120207       ENDSR;
024200120207
024300120207       //--------------------------------------------------------------
024400120510       // Inizializzazione videata W01
024500120207       //--------------------------------------------------------------
024600120510       BEGSR  sr_InzW01;
024700120207
024800120207         // - Pulizia videata
024900170302         clear  GOBSW01;
025000120620
025100120620         // - Imposto i campi video con i dati in ingresso
025200170307         w01DtEstr= %dec(DtEstr:*eur);
025300170307         w01Logica= Logica ;
025400170307         w01Obj   = Obj  ;
025500170302         w01Lib   = Lib  ;
025600170302         w01ObjT  = ObjT ;
025700170302
025800170302         // - Imposto i campi video con i dati di default
025900170303         w01LibSP = %subst(Dati:46:10);
026000170303         w01FilSP = %subst(Dati:36:10);
026100170303         w01MbrSP = %subst(Dati:56:10);
026200170302         w01LibSA = 'SRCTRAOLD';
026300170302         w01FilSA = 'SRCOLD' + %char(%subdt(%date():*Y));
026400170303         w01MbrSA = %subst(Dati:56:10);
026500170303         w01LibNP = %subst(Dati:46:10);
026600170303         w01FilNP = 'SRCNOTE';
026700170303         w01MbrNP = %subst(Dati:56:10);
026800170302         w01LibNA = 'SRCTRAOLD';
026900170302         w01FilNA = 'NOTOLD' + %char(%subdt(%date():*Y));
027000170303         w01MbrNA = %subst(Dati:56:10);
027100120207
027200120207       ENDSR;
027300120207
027400120207       //--------------------------------------------------------------
027500120510       // Gestione tasto funzionale F3 da videata W01                  ?
027600120510       // F3=Fine                                                      ?
027700120207       //--------------------------------------------------------------
027800120510       BEGSR  sr_F03W01;
027900120207
028000120207         // - Chiusura del programma
028100120207         $Fine = *on;
028200120207
028300120207       ENDSR;
028400120207
028500120207       //--------------------------------------------------------------
028600120510       // Gestione tasto funzionale F6 da videata W01
028700120208       // F6=Conferma
028800120207       //--------------------------------------------------------------
028900120510       BEGSR  sr_F06W01;
029000120405
029100170306         // doppia conferma
029200170308         if F06HI = *off;
029300170308           eval F06HI = *on;
029400170306           leavesr;
029500170306         else;
029600170308           eval F06HI = *off;
029700170306         endif;
029800170308
029900170306         // cancello oggetto
030000170306         exsr sr_dltobj;
030100170306
030200170306         // cancello rcd WAFOB
030300170306         exsr sr_dltrcdFOB;
030400170306
030500170306         if W01MbrSP <> *blank;
030600170308           wLibP  = W01LibSP;
030700170308           wFilP  = W01FilSP;
030800170308           wMbrP  = W01MbrSP;
030900170308           wLibA  = W01LibSA;
031000170308           wFilA  = W01FilSA;
031100170308           wMbrA  = W01MbrSA;
031200170306           // copio sorgente
031300170308           exsr sr_cpysrcmbr;
031400170306           // cancello sorgente
031500170308           exsr sr_rmvsrcmbr;
031600170308           // cancello sorgente £
031700170308           wMbrP  = %trim(W01MbrSP) + '£';
031800170308           exsr sr_rmvsrcmbr;
031900170308
032000170308           wLibP  = W01LibNP;
032100170308           wFilP  = W01FilNP;
032200170308           wMbrP  = W01MbrNP;
032300170308           wLibA  = W01LibNA;
032400170308           wFilA  = W01FilNA;
032500170308           wMbrA  = W01MbrNA;
032600170306           // copio nota
032700170308           exsr sr_cpysrcmbr;
032800170306           // cancello nota
032900170308           exsr sr_rmvsrcmbr;
033000170306         endif;
033100120215
033200120510         // Esco dal pgm (solo se non ho errori da mostrare)
033300120208         if ErrMsg = *off;
033400120208           $Fine = *on;
033500170308         else;
033600170308           VIDmsg = 'Segnalato almeno un errore durante la storicizzazione: +
033700170308                     controllare manualmente i vari passaggi';
033800120208         endif;
033900120207
034000120207       ENDSR;
034100120208
034200120207       //--------------------------------------------------------------
034300120510       // Gestione tasto funzionale F12 da videata W01
034400120208       // F12=Ritorno
034500120207       //--------------------------------------------------------------
034600120510       BEGSR  sr_F12W01;
034700120207
034800120207         // - Chiusura del programma
034900120207         $Fine = *on;
035000120207
035100120207       ENDSR;
035200120207
035300120207       //--------------------------------------------------------------
035400120510       // Controllo videata W01                                        ?
035500120207       //--------------------------------------------------------------
035600120510       BEGSR  sr_CtrW01;
035700120207
035800120620         //IndDspF  = *off;
035900120208
036000120207
036100120207       ENDSR;
036200170303
036300170303       //--------------------------------------------------------------
036400170303       // Cancella oggetto
036500170303       //--------------------------------------------------------------
036600170303       BEGSR  sr_DltObj;
036700170303
036800170303         QCmd =
036900170303          'DLTOBJ OBJ(' + %trim(W01Lib) ;
037000170303         QCmd = %trim(QCmd) + '/' + %trim(W01Obj);
037100170303         QCmd = %trim(QCmd) + ') OBJTYPE(' + %trim(W01ObjT);
037200170303         QCmd = %trim(QCmd) + ')';
037300170303
037400170303         select;
037500170303          // se sono su SETRAS
037600170303          when  %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
037700170303           // cancello oggetto sul sistema
037800170303           exsr sr_execCmd;
037900170308           if  Qusei <> *blank;
038000170308             ErrMsg = *on;
038100170308           endif;
038200170308
038300170303           // cancello oggetto su AS888
038400170308           QCmd = 'RUNRMTCMD CMD(''' + QCmd + ''') RMTLOCNAME(AS888)';
038500170303           exsr sr_execCmd;
038600170308           if  Qusei <> *blank;
038700170308             ErrMsg = *on;
038800170308           endif;
038900170303
039000170303          // se sono su AS888
039100170303          when  %subst ( currSysNeta : 1 : 6 ) = 'AS888 ';
039200170303           // cancello oggetto sul sistema
039300170303           exsr sr_execCmd;
039400170308           if  Qusei <> *blank;
039500170308             ErrMsg = *on;
039600170308           endif;
039700170303
039800170303         endsl;
039900170303
040000170303       ENDSR;
040100170306
040200170306       //--------------------------------------------------------------
040300170306       // Cancella record WAFOB00F
040400170306       //--------------------------------------------------------------
040500170306       BEGSR  sr_DltRcdFOB;
040600170306
040700170307         //la cancellazione del rcd la eseguo solo sul sistema in cui sono
040800170307
040900170306         wSQL =
041000170306          'delete from WAFOB00F +
041100170306           where +
041200170307           FOBDTESTR = ''' + %char(DtEstr) + ''' and +
041300170307           FOBLOGICA = ''' + %trim(W01Logica) + ''' and +
041400170307           FOBOBJ    = ''' + %trim(W01Obj) + ''' and +
041500170307           FOBLIB    = ''' + %trim(W01Lib) + ''' and +
041600170307           FOBOBJT   = ''' + %trim(W01ObjT) + '''';
041700170306
041800170307         exec sql execute immediate :wSQL;
041900170308         if SQLCod < 0 or SQLCod = 100;
042000170308           ErrMsg = *on;
042100170308         endif;
042200170306
042300170306       ENDSR;
042400170308
042500170308       //--------------------------------------------------------------
042600170308       // Copia membro sorgente
042700170308       //--------------------------------------------------------------
042800170308       BEGSR  sr_CpySrcMbr;
042900170308
043000170308         QCmd =
043100170308          'CPYSRCF FROMFILE(' + %trim(wLibP) + '/' +%trim(wFilP) + ')';
043200170308          QCmd = %trim(QCmd) + ' TOFILE(' + %trim(wLibA) + '/' +
043300170308                 %trim(wFilA) + ')';
043400170308          QCmd = %trim(QCmd) + ' FROMMBR(' + %trim(wMbrP) + ')';
043500170308          QCmd = %trim(QCmd) + ' TOMBR(' + %trim(wMbrA) + ')';
043600170308
043700170308         select;
043800170308          // se sono su SETRAS, il sorgente ce l'ho su AS888
043900170308          when  %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
044000170308           QCmd = 'RUNRMTCMD CMD(''' + QCmd + ''') RMTLOCNAME(AS888)';
044100170308           exsr sr_execCmd;
044200170308           if  Qusei <> *blank;
044300170308             ErrMsg = *on;
044400170308           endif;
044500170308
044600170308          // se sono su AS888, il sorgente ce l'ho qui
044700170308          when  %subst ( currSysNeta : 1 : 6 ) = 'AS888 ';
044800170308           // cancello oggetto sul sistema
044900170308           exsr sr_execCmd;
045000170308           if  Qusei <> *blank;
045100170308             ErrMsg = *on;
045200170308           endif;
045300170308
045400170308         endsl;
045500170308
045600170308       ENDSR;
045700170308
045800170308       //--------------------------------------------------------------
045900170308       // Cancella sorgente origine
046000170308       //--------------------------------------------------------------
046100170308       BEGSR  sr_RmvSrcMbr;
046200170308
046300170308         QCmd =
046400170308          'RMVM FILE(' + %trim(wLibP) + '/' +%trim(wFilP) + ') +
046500170308          MBR(' + %trim(wMbrP) + ')';
046600170308
046700170308         select;
046800170308          // se sono su SETRAS, il sorgente ce l'ho su AS888
046900170308          when  %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
047000170308           QCmd = 'RUNRMTCMD CMD(''' + QCmd + ''') RMTLOCNAME(AS888)';
047100170308           exsr sr_execCmd;
047200170308           if  Qusei <> *blank;
047300170308             ErrMsg = *on;
047400170308           endif;
047500170308
047600170308          // se sono su AS888, il sorgente ce l'ho qui
047700170308          when  %subst ( currSysNeta : 1 : 6 ) = 'AS888 ';
047800170308           // cancello oggetto sul sistema
047900170308           exsr sr_execCmd;
048000170308           if  Qusei <> *blank;
048100170308             ErrMsg = *on;
048200170308           endif;
048300170308
048400170308         endsl;
048500170308
048600170308       ENDSR;
048700170303
048800170303       //--------------------------------------------------------------
048900170303       // Esecuzione del comando (già impostato).                      ?
049000170303       //--------------------------------------------------------------
049100170303       BEGSR  sr_ExecCmd;
049200170303
049300170303         clear Qcap0100;
049400170303         Qcabcsdh = *off;
049500170303         Qcapa    = *off;
049600170303         Qcacmdss = *off;
049700170303         Qcaerved = *allX'00';
049800170303
049900170303         clear Qusec;
050000170303         Qusbprv  = %size(Qusec);
050100170303
050200170303         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
050300170303                           %size(Qcap0100) : 'CPOP0100' : *omit :
050400170303                           0 : 0 : Qusec);
050500170303
050600170303       ENDSR;
050700111128
050800111128       //--------------------------------------------------------------
050900120208       // Operazioni finali
051000111128       //--------------------------------------------------------------
051100111128       BEGSR  sr_RoutEnd;
051200111128
051300120208         // - Uscita
051400111128         return;
051500111128
051600111128       ENDSR;
051700111128
051800111128      /end-free
