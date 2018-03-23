000100090528       //--------------------------------------------------------------
000200090528       //?TIS784R - Crazione forzosa membri *FIRST                     ?
000300090528       //--------------------------------------------------------------
000400090528
000500090528     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600090529     h dftactgrp(*no) actgrp(*caller)
000700090528
000800090528       //--------------------------------------------------------------
000900090528       //?Dichiarazione file.                                          ?
001000090528       //--------------------------------------------------------------
001100090528
001200090528
001300090528       //--------------------------------------------------------------
001400090528       //?Definizione costanti.                                        ?
001500090528       //--------------------------------------------------------------
001600090528
001700090528     d c_MbrFirst      c                   const('MXXXXXXXX')
001800090528
001900090528       //--------------------------------------------------------------
002000090528       //?Definizione schiere.                                         ?
002100090528       //--------------------------------------------------------------
002200090528
002300090528       // - Elenco archivi nei quali gestire il 1° membro
002400090528     d FileDWLds       ds                  inz
002500090528     d   sk_FileDWL                  10    inz  dim(1000)
002600090528
002700090528       //--------------------------------------------------------------
002800090528       //?Definizione aree dati.                                       ?
002900090528       //--------------------------------------------------------------
003000090528
003100090528
003200090528       //--------------------------------------------------------------
003300090528       //?Definizione strutture dati.                                  ?
003400090528       //--------------------------------------------------------------
003500090528
003600090528
003700090528       //--------------------------------------------------------------
003800090528       //?Definizione variabili globali.                               ?
003900090528       //--------------------------------------------------------------
004000090528
004100090528       // - Indici di schiera
004200090528     d xx              s              5  0 inz
004300090529
004400090529       // - Parametri API QCAPCMD (Process Commands)
004500090529      /copy qsysinc/qrpglesrc,QCAPCMD
004600090529     d Qcmd            s            512    inz  varying
004700090528
004800090528       // - Parametri API QDBRTVFD (Retrieve Database File Description)
004900101005      /copy gaitrasrc/srctis7,QDBRTVFD2
005000090528     d Qdbrv           s           2048    inz                                  Receiver variable
005100090528
005200090528       // - Parametri API QUSRMBRD (Retrieve Member Description)
005300090528      /copy qsysinc/qrpglesrc,QUSRMBRD
005400090528     d Qmbrd01         s                   like(QUSM0200)  inz                  Receiver variable
005500090528     d Qmbrd02         s              9b 0 inz(%size(Qmbrd01))                  Length of receiver v
005600090528     d Qmbrd03         s              8    inz('MBRD0200')                      Format name
005700090528     d Qmbrd04         s             20    inz('          *LIBL     ')          Qualified database f
005800090528     d Qmbrd05         s             10    inz('*FIRST')                        Database member name
005900090528     d Qmbrd06         s              1    inz(*off)                            Override processing
006000090528
006100090528       // - Parametri gestione errori API.
006200090528      /copy qsysinc/qrpglesrc,QUSEC
006300090528
006400090528       //--------------------------------------------------------------
006500090528       //?Definizione procedure usate.                                 ?
006600090528       //--------------------------------------------------------------
006700090528
006800090528       // - Reperimento archivi di cui gestire il 1° membro
006900090528     d tis784r1        pr                  extpgm('TIS784R1')
007000090528     d   skFileDWL                         like(FileDWLds)
007100090529
007200090529       // - API QCAPCMD (Process Commands)
007300090529     d ProcessCommands...
007400090529     d                 pr                  extpgm('QCAPCMD')
007500090529     d  sourceCommandString...
007600090529     d                            32767a   const options(*varsize)
007700090529     d  sourceCommandStringLength...
007800090529     d                               10i 0 const
007900090529     d  optionsControlBlock...
008000090529     d                                     like(QCAP0100)
008100090529     d                                     const
008200090529     d                                     options(*varsize)
008300090529     d  optionsControlBlockLength...
008400090529     d                               10i 0 const
008500090529     d  optionsControlBlockFormat...
008600090529     d                                8a   const
008700090529     d  changedCommandString...
008800090529     d                            32767a   options(*varsize:*omit)
008900090529     d  lengthAvailableForChangedCommandString...
009000090529     d                               10i 0 const
009100090529     d  lengthOfChangedCommandStringAvailableToReturn...
009200090529     d                               10i 0 const
009300090529     d  errorCode...
009400090529     d                                     like(qusec)
009500090529     d                                     options(*varsize)
009600090528
009700090528       // - API QDBRTVFD (Retrieve Database File Description)
009800090528     d pr_QDbRtvFd     pr                   extpgm('QDBRTVFD')
009900090528     d   p_Qdbrv                            like(Qdbrv   )
010000090528     d   p_Qdblorv                          like(Qdblorv )
010100090528     d   p_Qdbrfal                          like(Qdbrfal )
010200090528     d   p_Qdbfn00                          like(Qdbfn00 )
010300090528     d   p_Qdbfaln                          like(Qdbfaln )
010400090528     d   p_Qdbrfn00                         like(Qdbrfn00)
010500090528     d   p_Qdbfilof                         like(Qdbfilof)
010600090528     d   p_Qdbystem                         like(Qdbystem)
010700090528     d   p_Qdbft                            like(Qdbft   )
010800090528     d   p_Qusec                            like(Qusec   )
010900090528
011000090528       // - API QUSRMBRD (Retrieve Member Description)
011100090528     d pr_QUsrMbrD     pr                   extpgm('QUSRMBRD')
011200090528     d   p_Qmbrd01                          like(Qmbrd01)
011300090528     d   p_Qmbrd02                          like(Qmbrd02)
011400090528     d   p_Qmbrd03                          like(Qmbrd03)
011500090528     d   p_Qmbrd04                          like(Qmbrd04)
011600090528     d   p_Qmbrd05                          like(Qmbrd05)
011700090528     d   p_Qmbrd06                          like(Qmbrd06)
011800090528
011900090528       //--------------------------------------------------------------
012000090528       //?Definizione key-list.                                        ?
012100090528       //--------------------------------------------------------------
012200090528
012300090528
012400090528       //--------------------------------------------------------------
012500090528       //?Riepilogo indicatori.                                        ?
012600090528       //--------------------------------------------------------------
012700090528       //--------------------------------------------------------------
012800090528
012900090528       //--------------------------------------------------------------
013000090528       //?M A I N - L I N E.                                           ?
013100090528       //--------------------------------------------------------------
013200090528
013300090528      /free
013400090528
013500090528       //?Operazioni iniziali?
013600090528       exsr  sr_RoutInz;
013700090528
013800090528       //?Ciclo di controllo del 1° membro di ciascun archivio?
013900090528       For  xx = 1  To  %elem(sk_FileDWL);
014000090528
014100090528         if  sk_FileDWL(xx) = *blank;
014200090528           leave;
014300090528         endif;
014400090528
014500090528         // - Elaborazione 1° membro di un singolo archivio
014600090528         exsr  sr_Elab1File;
014700090528
014800090528       endfor;
014900090528
015000090528       //?Operazioni finali?
015100090528       exsr  sr_RoutEnd;
015200090528
015300090528       //--------------------------------------------------------------
015400090528       //?Operazioni iniziali.                                         ?
015500090528       //--------------------------------------------------------------
015600090528       BEGSR  sr_RoutInz;
015700090528
015800090528         *inLR = *on;
015900090528
016000090528         // - Reperimento elenco archivi da gestire
016100090528         tis784r1 (FileDWLds);
016200090528
016300090528       ENDSR;
016400090528
016500090528       //--------------------------------------------------------------
016600090529       //?Elaborazione 1° membro di un singolo archivio.               ?
016700090528       //--------------------------------------------------------------
016800090528       BEGSR  sr_Elab1File;
016900090529
017000090529         //?a) Verifica se file esistente in *LIBL?
017100090529         //    (su AS888 ne mancano...)
017200090529         Qcmd = 'CHKOBJ obj('
017300090529                +       %trimr(sk_FileDWL(xx))
017400090529                +       ') objtype(*file)';
017500090529         exsr  sr_ExecCmd;
017600090529         if  Qusei <> *blank;
017700090529           leavesr;
017800090529         endif;
017900090528
018000090529         //?b) Verifica se file multimembro?
018100090529         //   ?SE file per un solo membro: => fine?
018200090528         clear  Qusec;
018300090528         clear  Qdbrv;
018400090528         Qdblorv  = %size(Qdbrv);
018500090528         clear  Qdbrfal;
018600090528         Qdbfn00  = 'FILD0100';
018700090528         Qdbfaln  = sk_FileDWL(xx) + '*LIBL     ';
018800090528         Qdbrfn00 = '*FIRST';
018900090528         Qdbfilof = *off;
019000090528         Qdbystem = '*LCL';
019100090528         Qdbft    = '*INT';
019200090528         pr_QDbRtvFd ( Qdbrv : Qdblorv : Qdbrfal : Qdbfn00 : Qdbfaln :
019300090528                    Qdbrfn00 : Qdbfilof : Qdbystem : Qdbft : Qusec );
019400090528         // - Reperimento informazioni di testata (nr max mbr).
019500090528         Qdbq25 = Qdbrv;
019600090528         if  Qdbfhmxm = 1;
019700090528           leavesr;
019800090528         endif;
019900090528
020000090529         //?c) SE file SENZA membri: Creazione del 1° membro MXXXXXXXX?
020100090529         //   ?(& => fine)?
020200100303         if  Qdbhmnum = *zeros;
020300090529           Qcmd = 'ADDPFM file('
020400090529                +         %trimr(sk_FileDWL(xx))
020500090529                +         ') mbr('
020600090529                +         c_MbrFirst
020700090529                +         ') text('' '')';
020800090529           exsr  sr_ExecCmd;
020900090529           leavesr;
021000090528         endif;
021100090528
021200090529         //?d) Identificazione del 1° membro?
021300090528         clear  Qusec;
021400090528         Qusbprv  = %size(Qusec);
021500090528         reset  Qusm0200;
021600090528         Qmbrd01  = Qusm0200;
021700090528         reset  Qmbrd02;
021800090528         reset  Qmbrd03;
021900090528         Qmbrd04  = Qdbfaln;
022000090528         reset  Qmbrd05;
022100090528         reset  Qmbrd06;
022200090528         pr_QUsrMbrD ( Qmbrd01 : Qmbrd02 : Qmbrd03 :
022300090528                       Qmbrd04 : Qmbrd05 : Qmbrd06 );
022400090528         Qusm0200 = Qmbrd01;
022500090528
022600090528         // - 1° membro già MXXXXXXXX (=> fine)
022700090528         if  Qusmn03 = c_MbrFirst;
022800090528           leavesr;
022900090528         endif;
023000090528
023100090529         //?e) Ridenominazione del 1° membro in MXXXXXXXX?
023200090528         Qcmd = 'RNMM file('
023300090528              +       %trimr(sk_FileDWL(xx))
023400090528              +       ') mbr('
023500090528              +       %trimr(Qusmn03)
023600090528              +       ') newmbr('
023700090528              +       c_MbrFirst
023800090528              +       ')';
023900090529         exsr  sr_ExecCmd;
024000090529         if  Qusei <> *blank;
024100090529           leavesr;
024200090529         endif;
024300090528
024400090529         //?f) Copia del 1° membro in uno nuovo (con il suo nome orig.)?
024500090528         Qcmd = 'CPYF fromfile('
024600090528              +       %trimr(sk_FileDWL(xx))
024700090528              +       ') tofile('
024800090528              +       %trimr(sk_FileDWL(xx))
024900090528              +       ') frommbr('
025000090528              +       c_MbrFirst
025100090528              +       ') tombr('
025200090528              +       %trimr(Qusmn03)
025300090528              +       ') mbropt(*add)';
025400090529         exsr  sr_ExecCmd;
025500090529         if  Qusei <> *blank;
025600090529           leavesr;
025700090529         endif;
025800090528
025900090529         //?g) Pulizia del 1° membro in MXXXXXXXX?
026000090528         Qcmd = 'CLRPFM file('
026100090528              +         %trimr(sk_FileDWL(xx))
026200090528              +         ') mbr('
026300090528              +         c_MbrFirst
026400090528              +         ')';
026500090529         exsr  sr_ExecCmd;
026600090529         if  Qusei <> *blank;
026700090529           leavesr;
026800090529         endif;
026900090528
027000090528       ENDSR;
027100090529
027200090529       //--------------------------------------------------------------
027300090529       //?Esecuzione del comando (già impostato).                      ?
027400090529       //--------------------------------------------------------------
027500090529       BEGSR  sr_ExecCmd;
027600090529
027700090529         clear Qcap0100;
027800090529         Qcabcsdh = *off;
027900090529         Qcapa    = *off;
028000090529         Qcacmdss = *off;
028100090529         Qcaerved = *allX'00';
028200090529
028300090529         clear Qusec;
028400090529         Qusbprv  = %size(Qusec);
028500090529
028600090529         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
028700090529                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
028800090529                           0 : 0 : Qusec);
028900090529
029000090529         if  Qusei <> *blank;
029100090529           exsr  sr_MsgErr;
029200090529         endif;
029300090529
029400090529       ENDSR;
029500090529
029600090529       //--------------------------------------------------------------
029700090529       //?Invio messaggio per segnalazione errori.                     ?
029800090529       //--------------------------------------------------------------
029900090529       BEGSR  sr_MsgErr;
030000090529
030100090529
030200090529       ENDSR;
030300090528
030400090528       //--------------------------------------------------------------
030500090529       //?Operazioni finali.                                           ?
030600090528       //--------------------------------------------------------------
030700090528       BEGSR  sr_RoutEnd;
030800090528
030900090528         return;
031000090528
031100090528       ENDSR;
031200090528
031300090528      /end-free
