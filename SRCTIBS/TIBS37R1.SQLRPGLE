000100090210     H DFTACTGRP(*NO) BNDDIR('TRUL':'QC2LE':'UBRTVNETA') ACTGRP(*NEW)
000200080930
000300080930      // ----------------------------------------------------------------------
000400080930      //
000500081014      //         Modifica password utenti di rete  ?
000600080930      //
000700080930      // ----------------------------------------------------------------------
000800080930      // ? DICHIARAZIONE DEI FILE ?
000900080930      // ----------------------------------------------------------------------
001000080930
001100080930
001200080930      // ? V A R I A B I L I ?
001300090209     d wPrgNum         s             10S 0 inz
001400081022     d wDomainNETBIOS  s            100A   inz('bartolini')
001500081022     d wDomain         s            100A   inz
001600081022     d wDomainOU       s            100A   inz
001700081022     d wRmtCmd         s           2000A   inz
001800090206     d wRmtHst         s            256A   inz
001900090209     d wStatus         s              1S 0 inz
002000121005     d wMessage        s            200A   inz
002100090209     d wDebug          s              5A   inz('false')
002200090210     d wLibreria       s             10A   inz('UNITRAGRP')
002300090211     d wTimestampInz   s               Z   inz
002400090211     d wTimestampEnd   s               Z   inz
002500090211     d wTimeout        s              2S 0 inz(30)
002600080930
002700081022
002800080930      // ? S C H I E R E ?
002900081022     d msg             s             78    dim(15) ctdata perrcd(1)
003000080930
003100081022
003200080930      // ? D S   I N T E R N E / E S T E R N E ?
003300081022     d trul33ds      e ds
003400090206     d kpjba         e ds
003500080930
003600081022
003700080930      // ? PROTOTIPI ?
003800081022      /copy gaitrasrc/srcprotopr,trul33r
003900090206      /copy gaitrasrc/srcprotopr,system
004000090206      /copy gaitrasrc/srcprotopr,UBVRYCNTN
004100090206      /copy gaitrasrc/srcprotopi,UBVRYCNTN
004200090210      /copy gaitrasrc/srcprotopr,UBRTVNETA
004300090210      /copy gaitrasrc/srcprotopi,UBRTVNETA
004400090206
004500090206
004600090206     D exceptionMessageID...
004700090206     D                 S              7A   IMPORT('_EXCP_MSGID')
004800090206
004900090206
005000080930
005100080930      // ----------------------------------------------------------------------
005200080930
005300080930     c     *entry        plist
005400081022     c                   parm                    InUsr            15
005500081022     c                   parm                    InPwd            15
005600081022     c                   parm                    InDomTyp          1
005700081022     c                   parm                    OutSts            1
005800081022     c                   parm                    OutMsg           80
005900080930
006000080930      /free
006100080930
006200080930       //?operazioni iniziali
006300081022         exsr routInz;
006400090212
006500090212
006600090212       //?verifica parametri ricevuti
006700090212         exsr chkParm;
006800090206
006900090206
007000090206       //?assegna il dominio rispetto al parametro typo-dominio ricevuto
007100090206         exsr calDom;
007200090206
007300090206
007400081022       //?stacca un progressivo
007500081022         exsr repPrg;
007600081022
007700081022
007800081022       //?effettua chiamata sincrona all'esecutore
007900081022         exsr exeCmd;
008000081014
008100080930
008200081022       //?chiusura *pgm
008300080930         *inlr = *on;
008400090212
008500090212
008600090212
008700090212
008800090212       // ----------------------------------------------------------------------
008900090212       //?Verifica parametri ricevuti in ingresso
009000090212       // ----------------------------------------------------------------------
009100090212       begsr chkParm;
009200090212
009300090212          if wStatus = 0;
009400090212             if InUsr     = *blanks or
009500090212                InPwd     = *blanks or
009600090212                InDomTyp  = *blanks;
009700090212                   wStatus = 1;
009800090212                   OutSts = '1';
009900090212                   OutMsg = msg(13);
010000090212             endif;
010100090212          endif;
010200090212
010300090212       endsr;
010400090209
010500090209
010600090209
010700090209
010800090209       // ----------------------------------------------------------------------
010900090209       //?Assegnazione dominio rispetto al parametro tipo-dominio ricevuto in input
011000090209       // ----------------------------------------------------------------------
011100090209       begsr calDom;
011200090209
011300090209          if wStatus = 0;
011400090209
011500090209          //?Tipo dominio 1 = bartolini.it
011600090209          //?Tipo dominio 2 = pda.bartolini.it
011700090209          select;
011800090209             when InDomTyp = '1';
011900090209                 wDomain   = 'bartolini.it';
012000090209                 wDomainOU = 'OU=Bart_Utenti';
012100090209             when InDomTyp = '2';
012200090209                 wDomain   = 'pda.bartolini.it';
012300090209                 wDomainOU = 'OU=Pda_Utenti,DC=pda';
012400090209             other;
012500090209                 wStatus = 1;
012600090209                 OutSts = '1';
012700090209                 OutMsg = msg(13);
012800090209          endsl;
012900090209
013000090209          endif;
013100090209
013200090209       endsr;
013300081014
013400081022
013500081022
013600081022
013700081014       // ----------------------------------------------------------------------
013800081022       //?Reperimento progressivo univoco
013900081014       // ----------------------------------------------------------------------
014000081022       begsr repPrg;
014100090209
014200090209          if wStatus = 0;
014300081022
014400090206          clear wPrgNum;
014500090206          clear trul33ds;
014600090206          I33OPE = *zeros;
014700090206          I33CNU = 303;
014800090206          I33NUM = 1;
014900090206          KPJBU = trul33ds;
015000090206          trul33r (KPJBA);
015100090206          trul33ds = KPJBU;
015200090206          if O33ERR = *zeros;
015300090206             wPrgNum = O33NRF;
015400090209          else;
015500090209             wStatus = 1;
015600090209             OutSts = '1';
015700090209             OutMsg = msg(12);
015800090206          endif;
015900090209
016000090209          endif;
016100081014
016200081014       endsr;
016300081022
016400081022
016500081014
016600080930
016700081022       // ----------------------------------------------------------------------
016800081022       //?Esecuzione comando remoto => chiamata a esecutore
016900081022       // ----------------------------------------------------------------------
017000081022       begsr exeCmd;
017100090206
017200090209          if wStatus = 0;
017300090206
017400090206          // Verifico innanzitutto la connettività
017500090206          inRmtSys = 'CEDFS5';
017600090206          UBVRYCNTN (inRmtSys:outEsito);
017700090206          if outEsito = '0';
017800090206          else;
017900090206             inRmtSys = 'CEDFS4';
018000090206             UBVRYCNTN (inRmtSys:outEsito);
018100090206             if outEsito = '0';
018200090206             else;
018300090209                inRmtSys = 'CEDFS6';
018400090206                UBVRYCNTN (inRmtSys:outEsito);
018500090206                if outEsito = '0';
018600120803                else;
018700120803                   inRmtSys = 'CEDBVPROCBO1';
018800120803                   UBVRYCNTN (inRmtSys:outEsito);
018900120803                   if outEsito = '0';
019000120803                   else;
019100120803                      inRmtSys = 'CEDBVPROCBO2';
019200120803                      UBVRYCNTN (inRmtSys:outEsito);
019300120803                      if outEsito = '0';
019400120803                      else;
019500120803                         inRmtSys = 'CEDBVPROCBO3';
019600120803                         UBVRYCNTN (inRmtSys:outEsito);
019700120803                         if outEsito = '0';
019800120803                         else;
019900120803                            inRmtSys = 'CEDBVPROCMI1';
020000120803                            UBVRYCNTN (inRmtSys:outEsito);
020100120803                            if outEsito = '0';
020200121005                            else;
020300121005                               inRmtSys = 'CEDBVPROCMI2';
020400121005                               UBVRYCNTN (inRmtSys:outEsito);
020500121005                               if outEsito = '0';
020600121005                               else;
020700121005                                  wStatus = 1;
020800121005                                  OutSts = '1';
020900121005                                  OutMsg = msg(14);
021000121005                               endif;
021100120803                            endif;
021200120803                         endif;
021300120803                      endif;
021400120803                   endif;
021500120803                endif;
021600090206             endif;
021700090206          endif;
021800090209          wRmtHst = inRmtSys;
021900090209
022000090209          endif;
022100090206
022200090209
022300090206
022400090206          // Se tutto ok => eseguo il comando sull'host remoto (in sincrono)
022500090206          if wStatus = 0;
022600090206
022700090206             //?compone il comando remoto da sottomettere
022800090206             exsr makCmd;
022900090206
023000090206             OutSts = %char(executeCommand(wRmtCmd));
023100090206             if OutSts <> *zeros;
023200090209                OutSts = '1';
023300090209                OutMsg = msg(01);
023400090206             else;
023500090206                if exceptionMessageID <> *blanks;
023600090209                   OutSts = '1';
023700090209                   OutMsg = msg(11) + exceptionMessageID;
023800090209                else;
023900090209                   //?verifica esito esecuzinoe comando remoto
024000090209                   exsr vryExe;
024100090206                endif;
024200090206             endif;
024300090209
024400090206          endif;
024500081022
024600081022       endsr;
024700090209
024800090209
024900090209
025000090209
025100090209       // ----------------------------------------------------------------------
025200090209       //?Composizione comando remoto da esguire
025300090209       // ----------------------------------------------------------------------
025400090209       begsr makCmd;
025500090209
025600090209          wRmtCmd = 'RUNRMTCMD CMD(''start'               + ' ' +
025700090209                    'ChgPwd.exe ' + %trim(InUsr)          + ' ' +
025800090209                                    %trim(InPwd)          + ' ' +
025900090209                                    %trim(wDomainNETBIOS) + ' ' +
026000090209                                    %trim(wDomain)        + ' ' +
026100090209                                    %trim(wDomainOU)      + ' ' +
026200090209                                    %trim(%char(wPrgNum)) + ' ' +
026300090209                                    %trim(wDebug)         + ' ' +
026400090210                                    %trim(wLibreria)      + ' ' +
026500090209                                    '0'                   + ' ' +
026600090209
026700090209                    ''') RMTLOCNAME(' + %trim(wRmtHst) + ' *IP)';
026800090209
026900090209       endsr;
027000090209
027100090209
027200090209
027300090209
027400090209       // ----------------------------------------------------------------------
027500090209       //?Verifica esito esecuzione comando remoto
027600090209       // ----------------------------------------------------------------------
027700090209       begsr vryExe;
027800090209
027900090209          // Imposto l'environment sql
028000090209          exec sql
028100090209               set option DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD;
028200090209
028300090210
028400090211          wTimestampInz = %timestamp;
028500090210          *in55 = *off;
028600090210
028700090210          dow *in55 = *off;
028800090210
028900090210              exec sql
029000121005                   select bs37sts,
029100121005                   case when bs37msg is not null then
029200121005                             bs37msg else '-' end case
029300121005                   into :wStatus, :wMessage
029400090210                     from tibs3700f
029500090210                     where bs37typ =  'LOG'    and
029600090212                           bs37key =  :wPrgNum;
029700090210
029800090210              // Se reperito il record => esco
029900090210              if sqlcode = *zeros;
030000090216                 if wStatus <> -1;
030100090216                    *in55 = *on;
030200090216                 endif;
030300090210              endif;
030400090210
030500090210              // Se in timeout => esco
030600090211              wTimestampEnd = %timestamp;
030700090211              if %diff(wTimestampEnd : wTimestampInz : *seconds) > wTimeout;
030800090211                 *in55 = *on;
030900090211              endif;
031000090210
031100090210          enddo;
031200090210
031300090210
031400090209
031500090209          if sqlcode <> *zeros;
031600090209                  OutSts = '1';
031700090209                  OutMsg = msg(01);
031800090209          else;
031900090209
032000090209             select;
032100090210
032200090209                when wStatus = 0;
032300090209                     OutSts = '0';
032400090209                     OutMsg = msg(15);
032500090210
032600090210                     exec sql
032700090210                          delete
032800090210                            from tibs3700f
032900090210                            where bs37typ = 'LOG' and
033000090212                            bs37key = :wPrgNum    and
033100090212                            bs37sts <> -1;
033200090210
033300090209                when wStatus = -1;
033400090209                     OutSts = '1';
033500090209                     OutMsg = msg(01);
033600090210
033700090209                other;
033800090209                     OutSts = %char(wStatus);
033900121005                //   OutMsg = msg(wStatus);
034000121005                     OutMsg = 'ERRORE => ' + %trim(wMessage);
034100090209             endsl;
034200090209
034300090209          endif;
034400090209
034500090209       endsr;
034600081022
034700081022
034800081022
034900081022
035000080930       // ----------------------------------------------------------------------
035100081022       //?Operazioni iniziali.
035200080930       // ----------------------------------------------------------------------
035300080930       begsr routinz;
035400090209
035500090209          // Inizializzazioni
035600090209          wStatus = 0;
035700090209          OutSts = ' ';      //*blanks=INZ '0'=OK '1'=ERR
035800090209          OutMsg = *blanks;
035900090210
036000090210          // Verifico il sistema AS/400 corrente
036100090210          if UBRTVNETA_Rtv(PiOutSysNeta) = 0;
036200090210             if %subst(PiOutSysNeta:1:6) = 'SETRAS';
036300090210                wLibreria  = 'UNITRAGRU';
036400090210             endif;
036500090210          else;
036600090210             wStatus = 1;
036700090210             OutSts = '1';
036800090210             OutMsg = msg(10);
036900090210          endif;
037000080930
037100081022       endsr;
037200080930
037300081022** MSG  Lungh. 78                                                            *
037400090206ERRORE => anomalia generica e modifica non effettuata.                        01
037500090206ERRORE => la password non soddisfa le regole di complessità.                  02
037600090206ERRORE => autorizzazioni insufficienti per completare la richiesta.           03
037700090206                                                                              04
037800090206                                                                              05
037900090206                                                                              06
038000090206                                                                              07
038100090206                                                                              08
038200090206                                                                              09
038300090210ERRORE => impossibile determinare il sistema AS/400 do origine                10
038400090209ERRORE => anomalia locale AS/400:                                             11
038500090209ERRORE => reperimento numeratore fallito                                      12
038600090209ERRORE => i parametri ricevuti non sono corretti                              13
038700090209ERRORE => operazione non riuscita x mancanza di connettività con l'host remoto14
038800090209SUCCESSO => operazione completata correttamente                               15
