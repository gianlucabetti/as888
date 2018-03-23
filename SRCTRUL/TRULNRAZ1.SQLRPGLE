000100081119     H DFTACTGRP(*NO)
000200061010
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
001500061010      //********************************************************************************************
001600061010      //
001700061010      // Definizione procedure usate.
001800061010      //
001900061010      //********************************************************************************************
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
004200061010      //********************************************************************************************
004300061010      //
004400061010      // Definizione variabili.
004500061010      //
004600061010      //********************************************************************************************
004700061011     D/COPY QSYSINC/QRPGLESRC,QCAPCMD
004800061011     D/COPY QSYSINC/QRPGLESRC,QUSEC
004900061011     D rtnEsito        S             10I 0
005000061011     D rtnOpCode       S             10A
005100061011     D rtnStatus       S             10I 0
005200061011     D commandString...
005300061010     D                 S            256A
005400061010     D lavoro...
005500061010     D                 S             10A
005600061010     D utente...
005700061010     D                 S             10A
005800061010     D numero...
005900061010     D                 S              6A
006000061011
006100061011      //*********************************************************************************************
006200061011      //
006300061011      // Definizione parametri procedura.
006400061011      //
006500061011      //*********************************************************************************************
006600061011     C     *ENTRY        PLIST
006700061011     C                   PARM                    rtnEsito
006800061011     C                   PARM                    rtnOpCode
006900061011     C                   PARM                    rtnStatus
007000061011
007100061011      /FREE
007200061011
007300061011       //********************************************************************************************
007400061011       //
007500061011       // Main.
007600061011       //
007700061011       //********************************************************************************************
008300061011
008400061011       MONITOR;
008401061011
008402061011         CLEAR rtnEsito;
008403061011         CLEAR rtnOpCode;
008404061011         CLEAR rtnStatus;
008500061011
008600061011         commandString = 'WRKOBJLCK OBJ(KPDT1) OBJTYPE(*DTAARA) OUTPUT(*PRINT)';
008700061011         RESET qcap0100;
008800061011         RESET qusec;
008900061011         ProcessCommands(commandString:%SIZE(commandString):qcap0100
009000061011         :%SIZE(qcap0100):'CPOP0100':*OMIT:0:0:qusec);
009100061011
009200061011         IF qusei <> *BLANK;
009300061011           rtnEsito = -1;
009400061011           rtnOpCode = qusei;
009500061011           EXSR uscita;
009600061011         ENDIF;
009700061011
009800061011         commandString = 'CHKOBJ OBJ(QTEMP/TMPDSPOLK) OBJTYPE(*FILE)';
009900061011         RESET qcap0100;
010000061011         RESET qusec;
010100061011         ProcessCommands(commandString:%SIZE(commandString):qcap0100
010200061011         :%SIZE(qcap0100):'CPOP0100':*OMIT:0:0:qusec);
010300061011
010400061011         SELECT;
010500061011           WHEN qusei = 'CPF9801'; // Non trovato l'oggetto nella libreria.
010600061011             commandString = 'CRTPF FILE(QTEMP/TMPDSPOLK) RCDLEN(132)';
010700061011             RESET qcap0100;
010800061011             RESET qusec;
010900061011             ProcessCommands(commandString:%SIZE(commandString):qcap0100
011000061011             :%SIZE(qcap0100):'CPOP0100':*OMIT:0:0:qusec);
011100061011           WHEN qusei = *BLANK; // Il file QTEMP/TMPDSPOLK esiste, lo pulisco.
011200061011             commandString = 'CLRPFM FILE(QTEMP/TMPDSPOLK)';
011300061011             RESET qcap0100;
011400061011             RESET qusec;
011500061011             ProcessCommands(commandString:%SIZE(commandString):qcap0100
011600061011             :%SIZE(qcap0100):'CPOP0100':*OMIT:0:0:qusec);
011700061011           OTHER;
011800061011             rtnEsito = -1;
011900061011             rtnOpCode = qusei;
012000061011             EXSR uscita;
012100061011         ENDSL;
012200061010
012300061011         commandString = 'CPYSPLF FILE(QPDSPOLK) TOFILE(QTEMP/TMPDSPOLK) +
012400061011         SPLNBR(*LAST)';
012500061011         RESET qcap0100;
012600061011         RESET qusec;
012700061011         ProcessCommands(commandString:%SIZE(commandString):qcap0100
012800061011         :%SIZE(qcap0100):'CPOP0100':*OMIT:0:0:qusec);
012900061011
013000061011         IF qusei <> *BLANK;
013100061011           rtnEsito = -1;
013200061011           rtnOpCode = qusei;
013300061011           EXSR uscita;
013400061011         ENDIF;
013500061010
013600061010      /END-FREE
013700061010     C/EXEC SQL
013800061010     C+ SELECT SUBSTR(TMPDSPOLK, 4, 10) LAVORO,
013900061010     C+        SUBSTR(TMPDSPOLK, 16, 10) UTENTE,
014000061010     C+        SUBSTR(TMPDSPOLK, 28, 6) NUMERO
014100061010     C+ INTO :lavoro, :utente, :numero
014200061010     C+ FROM QTEMP/TMPDSPOLK
014300061010     C+ WHERE SUBSTR(TMPDSPOLK, 47, 4) = 'CONG'
014400061010     C/END-EXEC
014500061010      /FREE
014600061010
014700061011         SELECT;
014800061011           WHEN sqlCode = 100;
014900061011           WHEN sqlCode < 0;
015000061011             rtnEsito = -1;
015100061011             rtnOpCode = 'SQLCODE';
015200061011             rtnStatus = sqlCode;
015300061011             EXSR uscita;
015400061011           OTHER;
015500061011             commandString = 'DSPJOB JOB(' + %TRIM(numero) + '/' +
015600061011             %TRIMR(utente) + '/' + %TRIM(lavoro) + ') OUTPUT(*PRINT)';
015700061011             RESET qcap0100;
015800061011             RESET qusec;
015900061011             ProcessCommands(commandString:%SIZE(commandString):qcap0100
016000061011             :%SIZE(qcap0100):'CPOP0100':*OMIT:0:0:qusec);
016100061011         ENDSL;
016200061011
016300061011         ON-ERROR;
016400061011           rtnEsito = -1;
016500061011           rtnOpCode = 'STATUS';
016600061011           rtnStatus = %STATUS;
016700061011           EXSR uscita;
016701061011
016800061011       ENDMON;
016900061010
017000061011       EXSR uscita;
017100061011
017200061011       //*******************************************************************************************
017300061011       //
017400061011       // Initialization subroutine
017500061011       //
017600061011       //*******************************************************************************************
017700061011       BEGSR *INZSR;
017800061011
017900061011         CLEAR qcap0100;
018000061011         qcabcsdh = IgnoreDBCSdata;
018100061011         qcapa = NeverPromptTheCommand;
018200061011         qcacmdss = CommandRunning;
018300061011         qcaerved = *ALLX'00';
018400061011         CLEAR qusec;
018500061011         qusbprv = %SIZE(qusec);
018600061011
018700061011       ENDSR;
018800061011
018900061011       //*******************************************************************************************
019000061011       //
019100061011       // Operazioni finali e fine programma.
019200061011       //
019300061011       //*******************************************************************************************
019400061011       BEGSR uscita;
019500061011
019600061011         MONITOR;
019700061011           commandString = 'DLTF FILE(QTEMP/TMPDSPOLK)';
019800061011           RESET qcap0100;
019900061011           RESET qusec;
020000061011           ProcessCommands(commandString:%SIZE(commandString):qcap0100
020100061011           :%SIZE(qcap0100):'CPOP0100':*OMIT:0:0:qusec);
020200061011           ON-ERROR;
020300061011         ENDMON;
020400061011
020500061011         IF rtnOpCode = *BLANK;
020600061011           rtnOpCode = 'DONE';
020700061011         ENDIF;
020800061011
020801061011         *INLR = *ON;
020900061011         RETURN;
021000061011
021100061011       ENDSR;
021200061011
021300061010      /END-FREE
