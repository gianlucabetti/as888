000100081119     H DFTACTGRP(*NO)
000200061011
000300061011      //*********************************************************************************************
000400061011      //
000500061011      // Definizione costanti.
000600061011      //
000700061011      //*********************************************************************************************
000800061011     D IgnoreDBCSdata...
000900061011     D                 C                   '0'
001000061011     D NeverPromptTheCommand...
001100061011     D                 C                   '0'
001200061011     D CommandRunning...
001300061011     D                 C                   '0'
001400061011
001500061011      //********************************************************************************************
001600061011      //
001700061011      // Definizione procedure usate.
001800061011      //
001900061011      //********************************************************************************************
002000061011     D ProcessCommands...
002100061011     D                 PR                  EXTPGM('QCAPCMD')
002200061011     D  sourceCommandString...
002300061011     D                            32767A   CONST OPTIONS(*VARSIZE)
002400061011     D  sourceCommandStringLength...
002500061011     D                               10I 0 CONST
002600061011     D  optionsControlBlock...
002700061011     D                                     LIKE(QCAP0100)
002800061011     D                                     CONST
002900061011     D  optionsControlBlockLength...
003000061011     D                               10I 0 CONST
003100061011     D  optionsControlBlockFormat...
003200061011     D                                8A   CONST
003300061011     D  changedCommandString...
003400061011     D                            32767A   OPTIONS(*VARSIZE:*OMIT)
003500061011     D  lengthAvailableForChangedCommandString...
003600061011     D                               10I 0 CONST
003700061011     D  lengthOfChangedCommandStringAvailableToReturn...
003800061011     D                               10I 0 CONST
003900061011     D  errorCode...
004000061011     D                                     LIKE(qusec)
004100061011
004200061011      //********************************************************************************************
004300061011      //
004400061011      // Definizione strutture dati.
004500061011      //
004600061011      //********************************************************************************************
004700061011     D psds           SDS
004800061011     D  lavoro               244    253A
004900061011     D  utente               254    263A
005000061011     D  numero               264    269A
005100061011
005200061011      //********************************************************************************************
005300061011      //
005400061011      // Definizione variabili.
005500061011      //
005600061011      //********************************************************************************************
005700061011     D/COPY QSYSINC/QRPGLESRC,QCAPCMD
005800061011     D/COPY QSYSINC/QRPGLESRC,QUSEC
005900061011     D rtnEsito        S             10I 0
006000061011     D rtnOpCode       S             10A
006100061011     D rtnStatus       S             10I 0
006200061011     D msg             S            512A   VARYING
006300061011     D sms             S            150A
006400061011     D commandString...
006500061011     D                 S            512A
006600061011
006700061011      //********************************************************************************************
006800061011      //
006900061011      // Definizione aree dati.
007000061011      //
007100061011      //********************************************************************************************
007200061011     D trulnrazb     E DS                  DTAARA(trulnrazba)
007300061011
007400061011      //*********************************************************************************************
007500061011      //
007600061011      // Definizione parametri procedura.
007700061011      //
007800061011      //*********************************************************************************************
007900061011     C     *ENTRY        PLIST
008000061011     C                   PARM                    rtnEsito
008100061011     C                   PARM                    rtnOpCode
008200061011     C                   PARM                    rtnStatus
008300061011
008400061011      /FREE
008500061011
008600061011       //********************************************************************************************
008700061011       //
008800061011       // Main.
008900061011       //
009000061011       //********************************************************************************************
009100061011
009200061011       MONITOR;
009300061011
009400061011         CLEAR rtnEsito;
009500061011         CLEAR rtnOpCode;
009600061011         CLEAR rtnStatus;
009700061011
009800061011         IN trulnrazb;
009900061011
010000061011         // Invio al massimo 1 messaggio ogni ora.
010100061011         IF %DIFF(%TIMESTAMP():mmtInvio:*HOURS) < 1;
010200061011           EXSR uscita;
010300061011         ENDIF;
010400061011
010500061011           IN *LOCK trulnrazb;
010600061011           mmtInvio = %TIMESTAMP();
010700061011           OUT trulnrazb;
010800061011
010900061011         msg = 'KPDT1 area dati allocata. Controllare stampe in coda '
011000061011         + %TRIMR(outqLib) + '/' + %TRIMR(outq) + '.';
011100061011
011200061011         // Invio messaggio a operatore di sistema.
011300061011         MONITOR;
011400061011           commandString = 'SNDMSG MSG(''' + msg + ''') TOUSR(*SYSOPR)';
011500061011           RESET qcap0100;
011600061011           RESET qusec;
011700061011           ProcessCommands(commandString:%SIZE(commandString):qcap0100
011800061011           :%SIZE(qcap0100):'CPOP0100':*OMIT:0:0:qusec);
011900061011           ON-ERROR;
012000061011         ENDMON;
012100061011
012200061011         // Invio messaggio a utente predefinito.
012300061011         MONITOR;
012400061011           IF utente400 <> *BLANK;
012500061011             commandString = 'SNDMSG MSG(''' + msg + ''') +
012600061011             TOUSR(' + %TRIMR(utente400) + ')';
012700061011             RESET qcap0100;
012800061011             RESET qusec;
012900061011             ProcessCommands(commandString:%SIZE(commandString):qcap0100
013000061011             :%SIZE(qcap0100):'CPOP0100':*OMIT:0:0:qusec);
013100061011           ENDIF;
013200061011           ON-ERROR;
013300061011         ENDMON;
013400061011
013500061011         msg += ':/PMessaggio inviato dal lavoro ' + numero + '/' +
013600061011         %TRIMR(utente) + '/' + %TRIMR(lavoro) + '.';
013700061011
013800061011         // Invio e-mail a ced.
013900061011         MONITOR;
014000061011           commandString = 'SNDDST TYPE(*LMSG) +
014100061019           TOINTNET((''cedalert@bartolini.it'')) +
014200061011           DSTD(''KPDT1 area dati allocata.'') +
014300061011           LONGMSG(''' + msg + ''') +
014400061011           IMPORTANCE(*HIGH) PTY(*HIGH)';
014500061011           RESET qcap0100;
014600061011           RESET qusec;
014700061011           ProcessCommands(commandString:%SIZE(commandString):qcap0100
014800061011           :%SIZE(qcap0100):'CPOP0100':*OMIT:0:0:qusec);
014900061011           ON-ERROR;
015000061011         ENDMON;
015100061011
015200061011         // Invio e-mail a indirizzo predefinito.
015300061011         MONITOR;
015400061011           IF mailto <> *BLANK;
015500061011             commandString = 'SNDDST TYPE(*LMSG) +
015600061011             TOINTNET((''' + %TRIMR(mailto) + ''')) +
015700061011             DSTD(''KPDT1 area dati allocata.'') +
015800061011             LONGMSG(''' + msg + ''') +
015900061011             IMPORTANCE(*HIGH) PTY(*HIGH)';
016000061011             RESET qcap0100;
016100061011             RESET qusec;
016200061011             ProcessCommands(commandString:%SIZE(commandString):qcap0100
016300061011             :%SIZE(qcap0100):'CPOP0100':*OMIT:0:0:qusec);
016400061011           ENDIF;
016500061011           ON-ERROR;
016600061011         ENDMON;
016700061011
016800061011         // Invio SMS a persona reperibile. Sottometto il lavoro perchè l'invio è lento.
016900061011         MONITOR;
017000061011           sms = 'KPDT1 area dati allocata. Controllare stampe in coda '
017100061011           + %TRIMR(outqLib) + '/' + %TRIMR(outq) + '. +
017200061011           Messaggio inviato dal lavoro ' + numero + '/' +
017300061011           %TRIMR(utente) + '/' + %TRIMR(lavoro) + '.';
017400061011           commandString = 'SBMJOB CMD(CALL PGM(TNSYEMSC) PARM(''' +
017500061011           %TRIMR(sms) + ''')) JOB(SMS_KPDT1) JOBPTY(1)';
017600061011           RESET qcap0100;
017700061011           RESET qusec;
017800061011           ProcessCommands(commandString:%SIZE(commandString):qcap0100
017900061011           :%SIZE(qcap0100):'CPOP0100':*OMIT:0:0:qusec);
018000061011           ON-ERROR;
018100061011         ENDMON;
018200061011
018300061011         ON-ERROR;
018400061011           rtnEsito = -1;
018500061011           rtnOpCode = 'STATUS';
018600061011           rtnStatus = %STATUS;
018700061011           EXSR uscita;
018800061011
018900061011       ENDMON;
019000061011
019100061011       EXSR uscita;
019200061011
019300061011       //*******************************************************************************************
019400061011       //
019500061011       // Initialization subroutine
019600061011       //
019700061011       //*******************************************************************************************
019800061011       BEGSR *INZSR;
019900061011
020000061011         CLEAR qcap0100;
020100061011         qcabcsdh = IgnoreDBCSdata;
020200061011         qcapa = NeverPromptTheCommand;
020300061011         qcacmdss = CommandRunning;
020400061011         qcaerved = *ALLX'00';
020500061011         CLEAR qusec;
020600061011         qusbprv = %SIZE(qusec);
020700061011
020800061011       ENDSR;
020900061011
021000061011       //*******************************************************************************************
021100061011       //
021200061011       // Operazioni finali e fine programma.
021300061011       //
021400061011       //*******************************************************************************************
021500061011       BEGSR uscita;
021600061011
021700061011         IF rtnOpCode = *BLANK;
021800061011           rtnOpCode = 'DONE';
021900061011         ENDIF;
022000061011
022100061011         *INLR = *ON;
022200061011         RETURN;
022300061011
022400061011       ENDSR;
022500061011
022600061011      /END-FREE
