000100090612       //==============================================================
000200090617       //?Copia dati di UNA vers.cappario in AS888 da SETRAS - Parte 1ª?
000300090616       //?         -=»> DA LANCIARE SU SETRAS <«=-                     ?
000400090612       //==============================================================
000500090612
000600090612     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700090612     h alwnull(*inputonly)
000800090612
000900090612       //--------------------------------------------------------------
001000090612       //?Dichiarazione file.                                          ?
001100090612       //--------------------------------------------------------------
001200090612
001300090612
001400090612       //--------------------------------------------------------------
001500090612       //?Definizione costanti.                                        ?
001600090612       //--------------------------------------------------------------
001700090612
001800090616     d c_RmtLocNAS888  c                   const('AS888')
001900090616     d c_SysAdrAS888   c                   const('10.46.0.20')
002000091204     d c_UsrPrfAS888   c                   const('EDPCED')
002100091204     d c_UsrPwdAS888   s             10    inz
002200090616     d c_LibSETRAS     c                   const('UNITRAGRU')
002300090612
002400090612       //--------------------------------------------------------------
002500090612       //?Definizione schiere.                                         ?
002600090612       //--------------------------------------------------------------
002700090612
002800090612     d sk_File         s             11    dim(12)  ctdata  perrcd(1)
002900090612     d sk_Ver          s             10    dim(%elem(sk_File))
003000090612     d                                     alt(sk_File)
003100090612
003200090612       //--------------------------------------------------------------
003300090612       //?Definizione aree dati.                                       ?
003400090612       //--------------------------------------------------------------
003500090612
003600090612
003700090612       //--------------------------------------------------------------
003800090612       //?Definizione strutture dati.                                  ?
003900090612       //--------------------------------------------------------------
004000090612
004100090612       // - Status ds
004200090612     d Status         sds
004300090612     d  SDSpgm           *proc
004400090617
004500090617       // - Parametri
004600090617     d KPJBA         e ds
004700090617     d TISIJ1ds      e ds                  inz
004800090612
004900090612       //--------------------------------------------------------------
005000090612       //?Definizione variabili globali.                               ?
005100090612       //--------------------------------------------------------------
005200090612
005300090617       // - Parametri gestiti (per i chiamati)
005400090617     d p_Versione      s             15  5 inz
005500090612
005600090612       // - Indici di schiera
005700090612     d xx              s              3  0 inz
005800090612
005900090612       // - Stringa SQL da eseguire
006000090612     d wSQL            s            512    inz  varying
006100090612
006200090612       // - Parametri API QCAPCMD (Process Commands)
006300090612      /copy qsysinc/qrpglesrc,QCAPCMD
006400090612     d Qcmd            s            512    inz  varying
006500090612
006600090612       // - Parametri gestione errori API.
006700090612      /copy qsysinc/qrpglesrc,QUSEC
006800090612
006900090612       //--------------------------------------------------------------
007000090616       //?Definizione prototipi usati.                                 ?
007100090612       //--------------------------------------------------------------
007200090612
007300090612       // - API QCAPCMD (Process Commands)
007400090612     d ProcessCommands...
007500090612     d                 pr                  extpgm('QCAPCMD')
007600090612     d  sourceCommandString...
007700090612     d                            32767a   const options(*varsize)
007800090612     d  sourceCommandStringLength...
007900090612     d                               10i 0 const
008000090612     d  optionsControlBlock...
008100090612     d                                     like(QCAP0100)
008200090612     d                                     const
008300090612     d                                     options(*varsize)
008400090612     d  optionsControlBlockLength...
008500090612     d                               10i 0 const
008600090612     d  optionsControlBlockFormat...
008700090612     d                                8a   const
008800090612     d  changedCommandString...
008900090612     d                            32767a   options(*varsize:*omit)
009000090612     d  lengthAvailableForChangedCommandString...
009100090612     d                               10i 0 const
009200090612     d  lengthOfChangedCommandStringAvailableToReturn...
009300090612     d                               10i 0 const
009400090612     d  errorCode...
009500090612     d                                     like(qusec)
009600090612     d                                     options(*varsize)
009700090612
009800090612       //--------------------------------------------------------------
009900090612       //?Definizione key-list.                                        ?
010000090612       //--------------------------------------------------------------
010100090612
010200090612
010300090612       //--------------------------------------------------------------
010400090612       //?Riepilogo indicatori.                                        ?
010500090612       //--------------------------------------------------------------
010600090612       //--------------------------------------------------------------
010700090612
010800090612       //--------------------------------------------------------------
010900090612       //?M A I N - L I N E.                                           ?
011000090612       //--------------------------------------------------------------
011100090612
011200090612     c     *Entry        plist
011300090617     c                   parm                    KPJBA
011400090612
011500090612      /free
011600090612
011700090612       //?Operazioni iniziali?
011800090612       exsr  sr_RoutInz;
011900090612
012000090612       //?Elaborazione file(s) cappario?
012100090616       exsr  sr_Elabora;
012200090612
012300090612       //?Operazioni finali?
012400090612       exsr  sr_RoutEnd;
012500090612
012600090612       //--------------------------------------------------------------
012700090612       //?Operazioni iniziali.                                         ?
012800090612       //--------------------------------------------------------------
012900090612       BEGSR  sr_RoutInz;
013000090612
013100090612         *inLR = *on;
013200090612
013300090612         //?Impostazione opzioni per SQL?
013400090612         exec sql   set  option  DynUsrPrf = *Owner,
013500090612                                 CloSqlCsr = *EndMod;
013600090617
013700090617         //?Impostazione parametri ricevuti?
013800090617         TISIJ1ds   = KPJBU;
013900090618         OSIJ1err   = *off;
014000090617         p_Versione = ISIJ1ver;
014100091204         c_UsrPwdAS888 = ISIJ1pwd;
014200090616
014300090616         //?Richiamo sincrono remoto per?
014400090616         //?cancellazione (precauzionale) libreria temporanea su AS888?
014500090617       //Qcmd = 'RUNRMTCMD cmd(''DLTLIB +
014600090617       //                      lib(' + %trimr(SDSpgm) + ')'') +
014700090617       //                  rmtlocname(''' + c_SysAdrAS888 + ''' *IP) +
014800090617       //                  rmtuser(' + c_UsrPrfAS888 + ') +
014900090617       //                  rmtpwd(' + c_UsrPwdAS888 + ')';
015000090617       //exsr  sr_ExecCmd;
015100090616
015200090616         //?Richiamo sincrono remoto per?
015300090616         //?creazione libreria temporanea su AS888?
015400090617       //Qcmd = 'RUNRMTCMD cmd(''CRTLIB +
015500090617       //                      lib(' + %trimr(SDSpgm) + ') +
015600090617       //                      text(''''*Temp x *Copy file da SETRAS+
015700090617       //                           '''')'') +
015800090617       //                  rmtlocname(''' + c_SysAdrAS888 + ''' *IP) +
015900090617       //                  rmtuser(' + c_UsrPrfAS888 + ') +
016000090617       //                  rmtpwd(' + c_UsrPwdAS888 + ')';
016100090617       //exsr  sr_ExecCmd;
016200090617       //if  Qusei <> *blank;
016300090617       //  dump(a);
016400090617       //  OSIJ1err = *on;
016500090617       //  exsr  sr_RoutEnd;
016600090617       //endif;
016700090617
016800090617         //?Richiamo sincrono remoto a *pgm TISIJ2R su AS888?
016900090617         //?per creare la libreria temporanea?
017000090617         Qcmd = 'RUNRMTCMD cmd(''CALL +
017100090617                               pgm(GAITRAOBJ/TISIJ2R) +
017200090617                               parm(''''1'''' ' +
017300090617                                    %trim(%editc(p_Versione : '3')) +
017400090617                                    ' ''''' + %trim(SDSpgm) + ''''')+
017500090617                                    '') +
017600090617                           rmtlocname(''' + c_SysAdrAS888 + ''' *IP) +
017700090617                           rmtuser(' + c_UsrPrfAS888 + ') +
017800090617                           rmtpwd(' + c_UsrPwdAS888 + ')';
017900090617         exsr  sr_ExecCmd;
018000090617         if  Qusei <> *blank;
018100090617           dump(a);
018200090617           OSIJ1err = *on;
018300090617           exsr  sr_RoutEnd;
018400090617         endif;
018500090612
018600090612       ENDSR;
018700090616
018800090616       //--------------------------------------------------------------
018900090616       //?Elaborazione file(s) cappario                                ?
019000090616       //--------------------------------------------------------------
019100090616       BEGSR  sr_Elabora;
019200090616
019300090617         IF  ISIJ1file = *blank;
019400090616
019500090616           //?Elaborazione di TUTTI i file cappario?
019600090616           For  xx = 1  To  %elem(sk_File);
019700090616
019800090616             if  sk_File(xx) = *blank;
019900090616               iter;
020000090616             endif;
020100090616
020200090616             // - Elaborazione singolo file cappario
020300090616             exsr  sr_Elab1File;
020400090616
020500090616           EndFor;
020600090616
020700090616         ELSE;
020800090616
020900090616           //?Elaborazione del SINGOLO file cappario (ricevuto *parm)?
021000090617           xx = %lookup(ISIJ1file : sk_File);
021100090616           if  xx > *zero;
021200090616             exsr  sr_Elab1File;
021300090617           else;
021400090617             OSIJ1err = *on;
021500090616           endif;
021600090616
021700090616         ENDIF;
021800090616
021900090616       ENDSR;
022000090612
022100090612       //--------------------------------------------------------------
022200090612       //?Elaborazione singolo file cappario.                          ?
022300090612       //--------------------------------------------------------------
022400090612       BEGSR  sr_Elab1File;
022500090612
022600090612         //?Cancellazione (precauzionale) nuovo cappario?
022700090616         Qcmd = 'DLTF file(QTEMP/' + %trimr(sk_File(xx)) + ')';
022800090612         exsr  sr_ExecCmd;
022900090612
023000090612         //?Creazione del nuovo cappario (vuoto) in QTEMP?
023100090616         Qcmd = 'CRTDUPOBJ obj(' + %trimr(sk_File(xx)) + ') +
023200090616                           fromlib(' + %trimr(c_LibSETRAS) + ') +
023300090612                           objtype(*file) +
023400090612                           tolib(QTEMP) +
023500090612                           newobj(*obj) +
023600090612                           data(*no) +
023700090612                           cst(*no) +
023800090612                           trg(*no)';
023900090612         exsr  sr_ExecCmd;
024000090612         // - In caso di errore: segnala (stampa) e chiude il pgm
024100090612         if  Qusei <> *blank;
024200090612           dump(a);
024300090617           OSIJ1err = *on;
024400090612           exsr  sr_RoutEnd;
024500090612         endif;
024600090612
024700090612         //?Inserimento dati nel file cappario in elaborazione?
024800090612         //?dal file cappario importato da SETRAS?
024900090616         wSQL = 'INSERT into QTEMP/' + %trimr(sk_File(xx)) +
025000090616                ' select * from ' + %trimr(c_LibSETRAS) + '/' +
025100090616                                    %trimr(sk_File(xx));
025200090612         if  sk_Ver(xx) <> *blank;
025300090615           wSQL += ' where ' + %trimr(sk_Ver(xx)) + '=' +
025400090617                               %editc(ISIJ1ver:'X');
025500090612         endif;
025600090612
025700090612         exec SQL   execute immediate :wSQL;
025800090612         // - In caso di errore: segnala (stampa), ma NON chiude il pgm
025900090612         //   e prosegue con il file successivo
026000090612         if  SqlCode < *zero;
026100090612           dump(a);
026200090617           OSIJ1err = *on;
026300090612         endif;
026400090612
026500090612       ENDSR;
026600090612
026700090612       //--------------------------------------------------------------
026800090612       //?Esecuzione del comando (già impostato).                      ?
026900090612       //--------------------------------------------------------------
027000090612       BEGSR  sr_ExecCmd;
027100090612
027200090612         clear Qcap0100;
027300090612         Qcabcsdh = *off;
027400090612         Qcapa    = *off;
027500090612         Qcacmdss = *off;
027600090612         Qcaerved = *allX'00';
027700090612
027800090612         clear Qusec;
027900090612         Qusbprv  = %size(Qusec);
028000090612
028100090612         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
028200090612                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
028300090612                           0 : 0 : Qusec);
028400090612
028500090612       ENDSR;
028600090612
028700090612       //--------------------------------------------------------------
028800090612       //?Operazioni finali.                                           ?
028900090612       //--------------------------------------------------------------
029000090612       BEGSR  sr_RoutEnd;
029100090616
029200090617         IF  OSIJ1err = *off;
029300090616
029400090616           //?Salvataggio/Ripristino archivi?
029500090612           Qcmd = 'SAVRSTOBJ obj(';
029600090617           IF  ISIJ1file = *blank;
029700090612             For  xx = 1  To  %elem(sk_File);
029800090612               if  sk_File(xx) <> *blank;
029900090612                 Qcmd += sk_File(xx);
030000090612               endif;
030100090612             EndFor;
030200090612           ELSE;
030300090616             Qcmd += %trimr(sk_File(xx));
030400090612           ENDIF;
030500090612           Qcmd += ') +
030600090612                    lib(QTEMP) +
030700090616                    rmtlocname(' + c_RmtLocNAS888 + ') +
030800090612                    objtype(*file) +
030900090616                    rstlib(' + %trimr(SDSpgm) + ')';
031000090612
031100090612           exsr  sr_ExecCmd;
031200090612           if  Qusei <> *blank;
031300090612             dump(a);
031400090617             OSIJ1err = *on;
031500090616             return;
031600090612           endif;
031700090616
031800090616           //?Richiamo sincrono remoto a *pgm TISIJ2R su AS888?
031900090616           //?per aggiornare gli archivi ricevuti?
032000090617           Qcmd = 'RUNRMTCMD cmd(''CALL +
032100090617                                 pgm(GAITRAOBJ/TISIJ2R) +
032200090617                                 parm(''''2'''' ' +
032300090617                                      %trim(%editc(p_Versione : '3')) +
032400090617                                      ' ''''' + %trim(SDSpgm) + '''''';
032500090617           if  ISIJ1file <> *blank;
032600090616             Qcmd += ' ''''' + sK_File(xx) + '''''';
032700090616           endif;
032800090616           Qcmd += ')'') +
032900090616                   rmtlocname(''' + c_SysAdrAS888 + ''' *IP) +
033000090616                   rmtuser(' + c_UsrPrfAS888 + ') +
033100090616                   rmtpwd(' + c_UsrPwdAS888 + ')';
033200090616           exsr  sr_ExecCmd;
033300090616           if  Qusei <> *blank;
033400090616             dump(a);
033500090617             OSIJ1err = *on;
033600090616             return;
033700090616           endif;
033800090616
033900090616           //?Cancellazione archivi inviati su AS888 da QTEMP in SETRAS?
034000090617           IF  ISIJ1file = *blank;
034100090616             For  xx = 1  To  %elem(sk_File);
034200090616               if  sk_File(xx) <> *blank;
034300090616                 Qcmd = 'DLTF file(QTEMP/' + %trimr(sk_File(xx)) + ')';
034400090616                 exsr  sr_ExecCmd;
034500090616               endif;
034600090616             EndFor;
034700090616           ELSE;
034800090616             Qcmd = 'DLTF file(QTEMP/' + %trimr(sk_File(xx)) + ')';
034900090616             exsr  sr_ExecCmd;
035000090616           ENDIF;
035100090616
035200090616         ENDIF;
035300090612
035400090612         //?Uscita?
035500090612         return;
035600090612
035700090612       ENDSR;
035800090612
035900090612      /end-free
036000090612
036100090612       //--------------------------------------------------------------
036200090612       //?Schiere a tempo di compilazione.                             ?
036300090612       //--------------------------------------------------------------
036400090612
036500090612** - sk_File / sk_Ver
036600090612AZCAE00F               1  *NO versione
036700090612AZCPC00F   CPCver      2
036800090612AZCPE00F   CPEver      3
036900090612AZCPL00F   CPLver      4
037000090612AZCPR00F   CPRver      5
037100090612AZCPS00F   CPSver      6
037200090618AZCPP00F               7  *NO versione
037300090618AZCPT00F   CPTver      8
037400090617                       9  AZCPV00F   CPVver      9
037500090618AZCAS00F              10  *NO versione
037600090618AZCEP00F              11  *NO versione
037700090618AZCLN00F              12  *NO versione
