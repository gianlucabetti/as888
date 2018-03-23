000100090612       //==============================================================
000200090616       //?Copia dati di UNA vers.cappario in AS888 da SETRAS - Parte 2ª?
000300090616       //?Richiamato dal *pgm TISIJ1R.                                 ?
000400090616       //?         -=»> DA LANCIARE SU SETRAS <«=-                     ?
000500090612       //==============================================================
000600090612
000700090612     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000800090612     h alwnull(*inputonly)
000900090612
001000090612       //--------------------------------------------------------------
001100090612       //?Dichiarazione file.                                          ?
001200090612       //--------------------------------------------------------------
001300090612
001400090616     d c_LibAS888      c                   const('UNITRAGRP')
001500090612
001600090612       //--------------------------------------------------------------
001700090612       //?Definizione costanti.                                        ?
001800090612       //--------------------------------------------------------------
001900090612
002000090612
002100090612       //--------------------------------------------------------------
002200090612       //?Definizione schiere.                                         ?
002300090612       //--------------------------------------------------------------
002400090612
002500090616     d sk_File         s             10    dim(12)  ctdata  perrcd(1)
002600090612     d sk_Ver          s             10    dim(%elem(sk_File))
002700090612     d                                     alt(sk_File)
002800090612
002900090612       //--------------------------------------------------------------
003000090612       //?Definizione aree dati.                                       ?
003100090612       //--------------------------------------------------------------
003200090612
003300090612
003400090612       //--------------------------------------------------------------
003500090612       //?Definizione strutture dati.                                  ?
003600090612       //--------------------------------------------------------------
003700090612
003800090612       // - Status ds
003900090612     d Status         sds
004000090612     d  SDSprm           *parms
004100090612
004200090612       //--------------------------------------------------------------
004300090612       //?Definizione variabili globali.                               ?
004400090612       //--------------------------------------------------------------
004500090612
004600090612       // - Parametri ricevuti
004700090616     d p_Passo         s              1
004800090615     d p_Versione      s             15  5
004900090616     d p_Libreria      s             10
005000090616     d p_File          s             10
005100090616
005200090616       // - Flag booleani
005300090616     d $Err            s               n   inz
005400090612
005500090612       // - Indici di schiera
005600090612     d xx              s              3  0 inz
005700090612
005800090612       // - Stringa SQL da eseguire
005900090612     d wSQL            s            512    inz  varying
006000090612
006100090612       // - Parametri API QCAPCMD (Process Commands)
006200090612      /copy qsysinc/qrpglesrc,QCAPCMD
006300090612     d Qcmd            s            512    inz  varying
006400090612
006500090612       // - Parametri gestione errori API.
006600090612      /copy qsysinc/qrpglesrc,QUSEC
006700090615
006800090615       // - Campi di comodo
006900090615     d w_Versione      s              5  0 inz
007000090612
007100090612       //--------------------------------------------------------------
007200090612       //?Definizione procedure usate.                                 ?
007300090612       //--------------------------------------------------------------
007400090612
007500090612       // - API QCAPCMD (Process Commands)
007600090612     d ProcessCommands...
007700090612     d                 pr                  extpgm('QCAPCMD')
007800090612     d  sourceCommandString...
007900090612     d                            32767a   const options(*varsize)
008000090612     d  sourceCommandStringLength...
008100090612     d                               10i 0 const
008200090612     d  optionsControlBlock...
008300090612     d                                     like(QCAP0100)
008400090612     d                                     const
008500090612     d                                     options(*varsize)
008600090612     d  optionsControlBlockLength...
008700090612     d                               10i 0 const
008800090612     d  optionsControlBlockFormat...
008900090612     d                                8a   const
009000090612     d  changedCommandString...
009100090612     d                            32767a   options(*varsize:*omit)
009200090612     d  lengthAvailableForChangedCommandString...
009300090612     d                               10i 0 const
009400090612     d  lengthOfChangedCommandStringAvailableToReturn...
009500090612     d                               10i 0 const
009600090612     d  errorCode...
009700090612     d                                     like(qusec)
009800090612     d                                     options(*varsize)
009900090612
010000090612       //--------------------------------------------------------------
010100090612       //?Definizione key-list.                                        ?
010200090612       //--------------------------------------------------------------
010300090612
010400090612
010500090612       //--------------------------------------------------------------
010600090612       //?Riepilogo indicatori.                                        ?
010700090612       //--------------------------------------------------------------
010800090612       //--------------------------------------------------------------
010900090612
011000090612       //--------------------------------------------------------------
011100090612       //?M A I N - L I N E.                                           ?
011200090612       //--------------------------------------------------------------
011300090612
011400090612     c     *Entry        plist
011500090616     c                   parm                    p_Passo
011600090612     c                   parm                    p_Versione
011700090616     c                   parm                    p_Libreria
011800090616     c                   parm                    p_File
011900090612
012000090612      /free
012100090612
012200090612       //?Operazioni iniziali?
012300090612       exsr  sr_RoutInz;
012400090612
012500090612       //?Ciclo di elaborazione?
012600090616       exsr  sr_Elabora;
012700090612
012800090612       //?Operazioni finali?
012900090612       exsr  sr_RoutEnd;
013000090612
013100090612       //--------------------------------------------------------------
013200090612       //?Operazioni iniziali.                                         ?
013300090612       //--------------------------------------------------------------
013400090612       BEGSR  sr_RoutInz;
013500090612
013600090612         *inLR = *on;
013700090612
013800090612         //?Impostazione opzioni per SQL?
013900090612         exec sql   set  option  DynUsrPrf = *Owner,
014000090612                                 CloSqlCsr = *EndMod;
014100090615
014200090615         //?Impostazione dei parametri numerici in campi numerici?
014300090615         w_Versione = p_Versione;
014400090612
014500090612       ENDSR;
014600090616
014700090616       //--------------------------------------------------------------
014800090616       //?Gestione passi richiesti                                     ?
014900090616       //--------------------------------------------------------------
015000090616       BEGSR  sr_Elabora;
015100090616
015200090616         Select;
015300090616
015400090616
015500090616           //?OPERAZIONI DA ESEGUIRE AL 1° PASSO:?
015600090616           When  p_Passo = '1';
015700090616
015800090616             //?Cancellazione (precauzionale) libreria temporanea?
015900090616             Qcmd = 'DLTLIB lib(' + %trimr(p_Libreria) + ')';
016000090616             exsr  sr_ExecCmd;
016100090616
016200090616             //?Creazione libreria temporanea?
016300090616             Qcmd = 'CRTLIB lib(' + %trimr(p_Libreria) + ') +
016400090617                            text(''*Temp x *Copy file da SETRAS'')';
016500090616             exsr  sr_ExecCmd;
016600090616             if  Qusei <> *blank;
016700090616               dump(a);
016800090616               $Err = *on;
016900090616             endif;
017000090616
017100090616             exsr  sr_RoutEnd;
017200090616
017300090616
017400090616           //?OPERAZIONI DA ESEGUIRE AL 2° PASSO:?
017500090616           When  p_Passo = '2';
017600090616
017700090616             //?Elaborazione archivi cappario?
017800090616             IF  SDSprm < 4;
017900090616
018000090616               For  xx = 1  To  %elem(sk_File);
018100090616
018200090616                 if  sk_File(xx) = *blank;
018300090616                   iter;
018400090616                 endif;
018500090616
018600090616                 // - Elaborazione singolo file cappario
018700090616                 exsr  sr_Elab1File;
018800090616
018900090616               EndFor;
019000090616
019100090616             ELSE;
019200090616
019300090616               // - Elaborazione singolo file cappario (ricevuto *parm)
019400090616               xx = %lookup(p_File : sk_File);
019500090616               if  xx > *zero;
019600090616                 exsr  sr_Elab1File;
019700090616               endif;
019800090616
019900090616             ENDIF;
020000090616
020100090616             //?Cancellazione libreria temporanea?
020200090616             if  Not $Err;
020300090616               Qcmd = 'DLTLIB lib(' + %trimr(p_Libreria) + ')';
020400090616               exsr  sr_ExecCmd;
020500090616             endif;
020600090616
020700090616         EndSl;
020800090616
020900090616       ENDSR;
021000090612
021100090612       //--------------------------------------------------------------
021200090612       //?Elaborazione singolo archivio.                               ?
021300090612       //--------------------------------------------------------------
021400090612       BEGSR  sr_Elab1File;
021500090612
021600090616         //?Cancellazione dei record della versione ricevuta?
021700090612         //?dal file cappario in elaborazione?
021800090616         wSQL = 'DELETE from ' + c_LibAS888 + '/' +
021900090616                                 %trimr(sk_File(xx));
022000090612         if  sk_Ver(xx) <> *blank;
022100090615           wSQL += ' where ' + %trimr(sk_Ver(xx)) + '=' +
022200090615                               %editc(w_Versione:'X');
022300090612         endif;
022400090612
022500090612         exec SQL   execute immediate :wSQL;
022600090612         // In caso di errore: segnala (stampa), ma NON chiude il pgm
022700090612         //   e prosegue con il file successivo
022800090612         if  SqlCode < *zero;
022900090612           dump(a);
023000090616           $Err = *on;
023100090612           leavesr;
023200090612         endif;
023300090612
023400090612         //?Inserimento dati nel file cappario in elaborazione?
023500090612         //?dal file cappario importato da SETRAS?
023600090616         wSQL = 'INSERT into ' + c_LibAS888 + '/' +
023700090616                                 %trimr(sk_File(xx)) +
023800090616                ' select * from ' + %trimr(p_Libreria) + '/' +
023900090612                                    %trimr(sk_File(xx));
024000090612
024100090612         exec SQL   execute immediate :wSQL;
024200090612         // In caso di errore: segnala (stampa), ma NON chiude il pgm
024300090612         //   e prosegue con il file successivo
024400090612         if  SqlCode < *zero;
024500090612           dump(a);
024600090616           $Err = *on;
024700090612           leavesr;
024800090612         endif;
024900090616
025000090616         //?Cancellazione del file cappario elaborato dalla libreria?
025100090616         //?ricevuta come parametro?
025200090616         Qcmd = 'DLTF file(' + %trimr(p_Libreria) + '/' +
025300090616                               %trimr(sk_File(xx)) + ')';
025400090616         exsr  sr_ExecCmd;
025500090616         // In caso di errore: segnala (stampa), ma NON chiude il pgm
025600090616         //   e prosegue con il file successivo
025700090616         if  Qusei <> *blank;
025800090616           dump(a);
025900090616           $Err = *on;
026000090616           leavesr;
026100090616         endif;
026200090612
026300090612       ENDSR;
026400090612
026500090612       //--------------------------------------------------------------
026600090612       //?Esecuzione del comando (già impostato).                      ?
026700090612       //--------------------------------------------------------------
026800090612       BEGSR  sr_ExecCmd;
026900090612
027000090612         clear Qcap0100;
027100090612         Qcabcsdh = *off;
027200090612         Qcapa    = *off;
027300090612         Qcacmdss = *off;
027400090612         Qcaerved = *allX'00';
027500090612
027600090612         clear Qusec;
027700090612         Qusbprv  = %size(Qusec);
027800090612
027900090612         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
028000090612                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
028100090612                           0 : 0 : Qusec);
028200090612
028300090612         //if  Qusei <> *blank;
028400090612         //  exsr  sr_MsgErr;
028500090612         //endif;
028600090612
028700090612       ENDSR;
028800090612
028900090612       //--------------------------------------------------------------
029000090612       //?Operazioni finali.                                           ?
029100090612       //--------------------------------------------------------------
029200090612       BEGSR  sr_RoutEnd;
029300090612
029400090612         //?Uscita?
029500090612         return;
029600090612
029700090612       ENDSR;
029800090612
029900090612      /end-free
030000090612
030100090612       //--------------------------------------------------------------
030200090612       //?Schiere a tempo di compilazione.                             ?
030300090612       //--------------------------------------------------------------
030400090612
030500090612** - sk_File / sk_Ver
030600090616AZCAE00F              1  *NO versione
030700090616AZCPC00F  CPCver      2
030800090616AZCPE00F  CPEver      3
030900090616AZCPL00F  CPLver      4
031000090616AZCPR00F  CPRver      5
031100090616AZCPS00F  CPSver      6
031200090618AZCPP00F              7  *NO versione
031300090618AZCPT00F  CPTver      8
031400090618                      9  AZCPV00F  CPVver      9
031500090618AZCAS00F             10  *NO versione
031600090618AZCEP00F             11  *NO versione
031700090618AZCLN00F             12  *NO versione
