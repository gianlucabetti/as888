000100090612       //==============================================================
000200090617       //?Copia dati di UNA vers.cappario in AS888 da SETRAS - Parte 1ª?
000300090616       //?         -=»> DA LANCIARE SU SETRAS <«=-                     ?
000400090612       //==============================================================
000500130131
000600130131       //--------------------------------------------------------------
000700130131       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800130131       //--------------------------------------------------------------
000900130131
001000130131      *PRM  dbgview(*source)
001100130131     /*END
001200130131
001300130131       //--------------------------------------------------------------
001400130131       //?Specifiche di controllo.                                     ?
001500130131       //--------------------------------------------------------------
001600090612
001700090612     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800090612     h alwnull(*inputonly)
001900090612
002000090612       //--------------------------------------------------------------
002100090612       //?Dichiarazione file.                                          ?
002200090612       //--------------------------------------------------------------
002300090612
002400090612
002500090612       //--------------------------------------------------------------
002600090612       //?Definizione costanti.                                        ?
002700090612       //--------------------------------------------------------------
002800090612
002900090616     d c_RmtLocNAS888  c                   const('AS888')
003000090616     d c_SysAdrAS888   c                   const('10.46.0.20')
003100090616     d c_LibSETRAS     c                   const('UNITRAGRU')
003200090612
003300090612       //--------------------------------------------------------------
003400090612       //?Definizione schiere.                                         ?
003500090612       //--------------------------------------------------------------
003600090612
003700090612     d sk_File         s             11    dim(12)  ctdata  perrcd(1)
003800090612     d sk_Ver          s             10    dim(%elem(sk_File))
003900090612     d                                     alt(sk_File)
004000090612
004100090612       //--------------------------------------------------------------
004200090612       //?Definizione aree dati.                                       ?
004300090612       //--------------------------------------------------------------
004400090612
004500090612
004600090612       //--------------------------------------------------------------
004700090612       //?Definizione strutture dati.                                  ?
004800090612       //--------------------------------------------------------------
004900090612
005000090612       // - Status ds
005100090612     d Status         sds
005200090612     d  SDSpgm           *proc
005300090617
005400090617       // - Parametri
005500090617     d KPJBA         e ds
005600090617     d TISIJ1ds      e ds                  inz
005700090612
005800090612       //--------------------------------------------------------------
005900090612       //?Definizione variabili globali.                               ?
006000090612       //--------------------------------------------------------------
006100090612
006200090617       // - Parametri gestiti (per i chiamati)
006300090617     d p_Versione      s             15  5 inz
006400130131     d p_UsrPrfAS888   s                   like(ISIJ1USR)  inz
006500130131     d p_UsrPwdAS888   s                   like(ISIJ1PWD)  inz
006600090612
006700090612       // - Indici di schiera
006800090612     d xx              s              3  0 inz
006900090612
007000090612       // - Stringa SQL da eseguire
007100090612     d wSQL            s            512    inz  varying
007200090612
007300090612       // - Parametri API QCAPCMD (Process Commands)
007400090612      /copy qsysinc/qrpglesrc,QCAPCMD
007500090612     d Qcmd            s            512    inz  varying
007600090612
007700090612       // - Parametri gestione errori API.
007800090612      /copy qsysinc/qrpglesrc,QUSEC
007900090612
008000090612       //--------------------------------------------------------------
008100090616       //?Definizione prototipi usati.                                 ?
008200090612       //--------------------------------------------------------------
008300090612
008400090612       // - API QCAPCMD (Process Commands)
008500090612     d ProcessCommands...
008600090612     d                 pr                  extpgm('QCAPCMD')
008700090612     d  sourceCommandString...
008800090612     d                            32767a   const options(*varsize)
008900090612     d  sourceCommandStringLength...
009000090612     d                               10i 0 const
009100090612     d  optionsControlBlock...
009200090612     d                                     like(QCAP0100)
009300090612     d                                     const
009400090612     d                                     options(*varsize)
009500090612     d  optionsControlBlockLength...
009600090612     d                               10i 0 const
009700090612     d  optionsControlBlockFormat...
009800090612     d                                8a   const
009900090612     d  changedCommandString...
010000090612     d                            32767a   options(*varsize:*omit)
010100090612     d  lengthAvailableForChangedCommandString...
010200090612     d                               10i 0 const
010300090612     d  lengthOfChangedCommandStringAvailableToReturn...
010400090612     d                               10i 0 const
010500090612     d  errorCode...
010600090612     d                                     like(qusec)
010700090612     d                                     options(*varsize)
010800090612
010900090612       //--------------------------------------------------------------
011000090612       //?Definizione key-list.                                        ?
011100090612       //--------------------------------------------------------------
011200090612
011300090612
011400090612       //--------------------------------------------------------------
011500090612       //?Riepilogo indicatori.                                        ?
011600090612       //--------------------------------------------------------------
011700090612       //--------------------------------------------------------------
011800090612
011900090612       //--------------------------------------------------------------
012000090612       //?M A I N - L I N E.                                           ?
012100090612       //--------------------------------------------------------------
012200090612
012300090612     c     *Entry        plist
012400090617     c                   parm                    KPJBA
012500090612
012600090612      /free
012700090612
012800090612       //?Operazioni iniziali?
012900090612       exsr  sr_RoutInz;
013000090612
013100090612       //?Elaborazione file(s) cappario?
013200090616       exsr  sr_Elabora;
013300090612
013400090612       //?Operazioni finali?
013500090612       exsr  sr_RoutEnd;
013600090612
013700090612       //--------------------------------------------------------------
013800090612       //?Operazioni iniziali.                                         ?
013900090612       //--------------------------------------------------------------
014000090612       BEGSR  sr_RoutInz;
014100090612
014200090612         *inLR = *on;
014300090612
014400090612         //?Impostazione opzioni per SQL?
014500090612         exec sql   set  option  DynUsrPrf = *Owner,
014600090612                                 CloSqlCsr = *EndMod;
014700090617
014800090617         //?Impostazione parametri ricevuti?
014900090617         TISIJ1ds   = KPJBU;
015000090618         OSIJ1err   = *off;
015100090617         p_Versione = ISIJ1ver;
015200130131         p_UsrPrfAS888 = ISIJ1usr;
015300130131         p_UsrPwdAS888 = ISIJ1pwd;
015400090616
015500090616         //?Richiamo sincrono remoto per?
015600090616         //?cancellazione (precauzionale) libreria temporanea su AS888?
015700090617       //Qcmd = 'RUNRMTCMD cmd(''DLTLIB +
015800090617       //                      lib(' + %trimr(SDSpgm) + ')'') +
015900090617       //                  rmtlocname(''' + c_SysAdrAS888 + ''' *IP) +
016000130131       //                  rmtuser(' + p_UsrPrfAS888 + ') +
016100130131       //                  rmtpwd(' + p_UsrPwdAS888 + ')';
016200090617       //exsr  sr_ExecCmd;
016300090616
016400090616         //?Richiamo sincrono remoto per?
016500090616         //?creazione libreria temporanea su AS888?
016600090617       //Qcmd = 'RUNRMTCMD cmd(''CRTLIB +
016700090617       //                      lib(' + %trimr(SDSpgm) + ') +
016800090617       //                      text(''''*Temp x *Copy file da SETRAS+
016900090617       //                           '''')'') +
017000090617       //                  rmtlocname(''' + c_SysAdrAS888 + ''' *IP) +
017100130131       //                  rmtuser(' + p_UsrPrfAS888 + ') +
017200130131       //                  rmtpwd(' + p_UsrPwdAS888 + ')';
017300090617       //exsr  sr_ExecCmd;
017400090617       //if  Qusei <> *blank;
017500090617       //  dump(a);
017600090617       //  OSIJ1err = *on;
017700090617       //  exsr  sr_RoutEnd;
017800090617       //endif;
017900090617
018000090617         //?Richiamo sincrono remoto a *pgm TISIJ2R su AS888?
018100090617         //?per creare la libreria temporanea?
018200090617         Qcmd = 'RUNRMTCMD cmd(''CALL +
018300090617                               pgm(GAITRAOBJ/TISIJ2R) +
018400090617                               parm(''''1'''' ' +
018500090617                                    %trim(%editc(p_Versione : '3')) +
018600090617                                    ' ''''' + %trim(SDSpgm) + ''''')+
018700090617                                    '') +
018800090617                           rmtlocname(''' + c_SysAdrAS888 + ''' *IP) +
018900130131                           rmtuser(' + p_UsrPrfAS888 + ') +
019000130131                           rmtpwd(' + p_UsrPwdAS888 + ')';
019100090617         exsr  sr_ExecCmd;
019200090617         if  Qusei <> *blank;
019300090617           dump(a);
019400090617           OSIJ1err = *on;
019500090617           exsr  sr_RoutEnd;
019600090617         endif;
019700090612
019800090612       ENDSR;
019900090616
020000090616       //--------------------------------------------------------------
020100090616       //?Elaborazione file(s) cappario                                ?
020200090616       //--------------------------------------------------------------
020300090616       BEGSR  sr_Elabora;
020400090616
020500090617         IF  ISIJ1file = *blank;
020600090616
020700090616           //?Elaborazione di TUTTI i file cappario?
020800090616           For  xx = 1  To  %elem(sk_File);
020900090616
021000090616             if  sk_File(xx) = *blank;
021100090616               iter;
021200090616             endif;
021300090616
021400090616             // - Elaborazione singolo file cappario
021500090616             exsr  sr_Elab1File;
021600090616
021700090616           EndFor;
021800090616
021900090616         ELSE;
022000090616
022100090616           //?Elaborazione del SINGOLO file cappario (ricevuto *parm)?
022200090617           xx = %lookup(ISIJ1file : sk_File);
022300090616           if  xx > *zero;
022400090616             exsr  sr_Elab1File;
022500090617           else;
022600090617             OSIJ1err = *on;
022700090616           endif;
022800090616
022900090616         ENDIF;
023000090616
023100090616       ENDSR;
023200090612
023300090612       //--------------------------------------------------------------
023400090612       //?Elaborazione singolo file cappario.                          ?
023500090612       //--------------------------------------------------------------
023600090612       BEGSR  sr_Elab1File;
023700090612
023800090612         //?Cancellazione (precauzionale) nuovo cappario?
023900090616         Qcmd = 'DLTF file(QTEMP/' + %trimr(sk_File(xx)) + ')';
024000090612         exsr  sr_ExecCmd;
024100090612
024200090612         //?Creazione del nuovo cappario (vuoto) in QTEMP?
024300090616         Qcmd = 'CRTDUPOBJ obj(' + %trimr(sk_File(xx)) + ') +
024400090616                           fromlib(' + %trimr(c_LibSETRAS) + ') +
024500090612                           objtype(*file) +
024600090612                           tolib(QTEMP) +
024700090612                           newobj(*obj) +
024800090612                           data(*no) +
024900090612                           cst(*no) +
025000090612                           trg(*no)';
025100090612         exsr  sr_ExecCmd;
025200090612         // - In caso di errore: segnala (stampa) e chiude il pgm
025300090612         if  Qusei <> *blank;
025400090612           dump(a);
025500090617           OSIJ1err = *on;
025600090612           exsr  sr_RoutEnd;
025700090612         endif;
025800090612
025900090612         //?Inserimento dati nel file cappario in elaborazione?
026000090612         //?dal file cappario importato da SETRAS?
026100090616         wSQL = 'INSERT into QTEMP/' + %trimr(sk_File(xx)) +
026200090616                ' select * from ' + %trimr(c_LibSETRAS) + '/' +
026300090616                                    %trimr(sk_File(xx));
026400090612         if  sk_Ver(xx) <> *blank;
026500090615           wSQL += ' where ' + %trimr(sk_Ver(xx)) + '=' +
026600090617                               %editc(ISIJ1ver:'X');
026700090612         endif;
026800090612
026900090612         exec SQL   execute immediate :wSQL;
027000090612         // - In caso di errore: segnala (stampa), ma NON chiude il pgm
027100090612         //   e prosegue con il file successivo
027200090612         if  SqlCode < *zero;
027300090612           dump(a);
027400090617           OSIJ1err = *on;
027500090612         endif;
027600090612
027700090612       ENDSR;
027800090612
027900090612       //--------------------------------------------------------------
028000090612       //?Esecuzione del comando (già impostato).                      ?
028100090612       //--------------------------------------------------------------
028200090612       BEGSR  sr_ExecCmd;
028300090612
028400090612         clear Qcap0100;
028500090612         Qcabcsdh = *off;
028600090612         Qcapa    = *off;
028700090612         Qcacmdss = *off;
028800090612         Qcaerved = *allX'00';
028900090612
029000090612         clear Qusec;
029100090612         Qusbprv  = %size(Qusec);
029200090612
029300090612         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
029400090612                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
029500090612                           0 : 0 : Qusec);
029600090612
029700090612       ENDSR;
029800090612
029900090612       //--------------------------------------------------------------
030000090612       //?Operazioni finali.                                           ?
030100090612       //--------------------------------------------------------------
030200090612       BEGSR  sr_RoutEnd;
030300090616
030400090617         IF  OSIJ1err = *off;
030500090616
030600090616           //?Salvataggio/Ripristino archivi?
030700090612           Qcmd = 'SAVRSTOBJ obj(';
030800090617           IF  ISIJ1file = *blank;
030900090612             For  xx = 1  To  %elem(sk_File);
031000090612               if  sk_File(xx) <> *blank;
031100090612                 Qcmd += sk_File(xx);
031200090612               endif;
031300090612             EndFor;
031400090612           ELSE;
031500090616             Qcmd += %trimr(sk_File(xx));
031600090612           ENDIF;
031700090612           Qcmd += ') +
031800090612                    lib(QTEMP) +
031900090616                    rmtlocname(' + c_RmtLocNAS888 + ') +
032000090612                    objtype(*file) +
032100090616                    rstlib(' + %trimr(SDSpgm) + ')';
032200090612
032300090612           exsr  sr_ExecCmd;
032400090612           if  Qusei <> *blank;
032500090612             dump(a);
032600090617             OSIJ1err = *on;
032700090616             return;
032800090612           endif;
032900090616
033000090616           //?Richiamo sincrono remoto a *pgm TISIJ2R su AS888?
033100090616           //?per aggiornare gli archivi ricevuti?
033200090617           Qcmd = 'RUNRMTCMD cmd(''CALL +
033300090617                                 pgm(GAITRAOBJ/TISIJ2R) +
033400090617                                 parm(''''2'''' ' +
033500090617                                      %trim(%editc(p_Versione : '3')) +
033600090617                                      ' ''''' + %trim(SDSpgm) + '''''';
033700090617           if  ISIJ1file <> *blank;
033800090616             Qcmd += ' ''''' + sK_File(xx) + '''''';
033900090616           endif;
034000090616           Qcmd += ')'') +
034100090616                   rmtlocname(''' + c_SysAdrAS888 + ''' *IP) +
034200130131                   rmtuser(' + p_UsrPrfAS888 + ') +
034300130131                   rmtpwd(' + p_UsrPwdAS888 + ')';
034400090616           exsr  sr_ExecCmd;
034500090616           if  Qusei <> *blank;
034600090616             dump(a);
034700090617             OSIJ1err = *on;
034800090616             return;
034900090616           endif;
035000090616
035100090616           //?Cancellazione archivi inviati su AS888 da QTEMP in SETRAS?
035200090617           IF  ISIJ1file = *blank;
035300090616             For  xx = 1  To  %elem(sk_File);
035400090616               if  sk_File(xx) <> *blank;
035500090616                 Qcmd = 'DLTF file(QTEMP/' + %trimr(sk_File(xx)) + ')';
035600090616                 exsr  sr_ExecCmd;
035700090616               endif;
035800090616             EndFor;
035900090616           ELSE;
036000090616             Qcmd = 'DLTF file(QTEMP/' + %trimr(sk_File(xx)) + ')';
036100090616             exsr  sr_ExecCmd;
036200090616           ENDIF;
036300090616
036400090616         ENDIF;
036500090612
036600090612         //?Uscita?
036700090612         return;
036800090612
036900090612       ENDSR;
037000090612
037100090612      /end-free
037200090612
037300090612       //--------------------------------------------------------------
037400090612       //?Schiere a tempo di compilazione.                             ?
037500090612       //--------------------------------------------------------------
037600090612
037700090612** - sk_File / sk_Ver
037800090612AZCAE00F               1  *NO versione
037900090612AZCPC00F   CPCver      2
038000090612AZCPE00F   CPEver      3
038100090612AZCPL00F   CPLver      4
038200090612AZCPR00F   CPRver      5
038300090612AZCPS00F   CPSver      6
038400090618AZCPP00F               7  *NO versione
038500090618AZCPT00F   CPTver      8
038600090617                       9  AZCPV00F   CPVver      9
038700090618AZCAS00F              10  *NO versione
038800090618AZCEP00F              11  *NO versione
038900090618AZCLN00F              12  *NO versione
