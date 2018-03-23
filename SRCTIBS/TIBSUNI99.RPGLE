000100080515     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('TIBS':'QC2LE')
000200080418
000300080515     Fanuni01l  IF   E           K DISK    USROPN
000400080515     Fndreg00f  IF   E             DISK    USROPN
000500080418     FqSysPrt   O    F  132        PRINTER
000600080418
000700080430     ***************************************************************************
000800080430     **
000900080430     ** Definizione prototipi procedure.
001000080430     **
001100080430     ***************************************************************************
001200080515     D/COPY GAITRASRC/SRCPROTOPR,TIBSUNI00
001300080430
001400080429     ***************************************************************************
001500080429     **
001600080429     ** Definizione costanti.
001700080429     **
001800080429     ***************************************************************************
001900080515     D UNITA_ESITO_ERRORE_GENERICO...
002000080429     D                 C                   1
002100080515     D UNITA_ESITO_ERRORE_UNITA_CODICENONVALIDO...
002200080429     D                 C                   2
002300080515     D UNITA_ESITO_ERRORE_UNITA_NONESISTE...
002400080429     D                 C                   3
002500080515     D UNITA_ESITO_ERRORE_OPCODE_SCONOSCIUTO...
002600080429     D                 C                   4
002700080515     D UNITA_OPCODE_GETANAGRAFICAUNITA...
002800080515     D                 C                   'GETANUNI00'
002900080515     D UNITA_OPCODE_GETDESCRIZIONEUNITA...
003000080515     D                 C                   'GETDESBREV'
003100080515     D UNITA_OPCODE_INIT...
003200080429     D                 C                   'INIT'
003300080515     D UNITA_OPCODE_ISINIT...
003400080429     D                 C                   'ISINIT'
003500080515     D UNITA_OPCODE_FINALIZE...
003600080429     D                 C                   'FINALIZE'
003700080429
003800080429     ***************************************************************************
003900080429     **
004000080429     ** Definizione strutture dati.
004100080429     **
004200080429     ***************************************************************************
004300080515     D anuni00f      E DS                  QUALIFIED BASED(nullPtr)
004400080515     D tibsuni01     E DS                  QUALIFIED
004500080515     D tibsuni02     E DS                  QUALIFIED
004600080515     D  societa      E                     INZ('201')
004700080515     D  dtValSoc     E                     INZ(*SYS)
004800080430
004900080429     ***************************************************************************
005000080429     **
005100080429     ** Definizione variabili globali.
005200080429     **
005300080429     ***************************************************************************
005400080515     D descrizioneUni  S                   LIKE(anuni00f.uniDesBrev)
005500080429     D inizio          S               Z
005600080429     D fine            S               Z
005700080515     D countNdreg      S             10U 0
005800080429     D durata          S              5U 0
005900080429     D rqsOpCode       S             10A
006000080429     D rpyEsito        S             10I 0
006100080515     D rqsData         S             11A
006200080429     D rqsDataLen      S             10I 0
006300080515     D rpyData         S             31A
006400080429     D rpyDataLen      S             10I 0
006500080429
006600080429      /FREE
006700080429
006800080429       *INLR = *ON;
006900080429
007000080430       //***********************************************************************
007100080430       //
007200080430       // Test con chiamata al modulo.
007300080430       //
007400080430       //***********************************************************************
007500080430
007600080515       RESET countNdreg;
007700080429       inizio = %TIMESTAMP();
007800080429       EXCEPT logInizio;
007900080515       OPEN ndreg00f;
008000080515       IF NOT Unita_isInit();
008100080515         Unita_init('201' : %DATE());
008200080429       ENDIF;
008300080515       DOU %EOF(ndreg00f);
008400080515         READ ndreg00f;
008500080429         IF %EOF();
008600080429           LEAVE;
008700080429         ENDIF;
008800080515         countNdreg += 1;
008900080515         Unita_getDescrizioneUnita(regSocieta : regUnita : descrizioneUni);
009000080429       ENDDO;
009100080515       Unita_finalize();
009200080515       CLOSE ndreg00f;
009300080429       fine = %TIMESTAMP();
009400080429       durata = %DIFF(fine : inizio : *SECONDS);
009500080429       EXCEPT logFine;
009600080430
009700080430      /END-FREE
009800080430
009900080430     ***************************************************************************
010000080430     **
010100080430     ** Test con chiamata al programma.
010200080430     **
010300080430     ***************************************************************************
010400080430
010500080515     C                   RESET                   countNdreg
010600080430     C                   EVAL      inizio = %TIMESTAMP()
010700080430     C                   EXCEPT    logInizio
010800080515     C                   OPEN      ndreg00f
010900080430     C
011000080515     C                   EVAL      rqsDataLen = %SIZE(tibsuni02)
011100080515     C                   CALL      'TIBSUNI00'
011200080430     C                   PARM      'INIT'        rqsOpCode
011300080430     C                   PARM                    rpyEsito
011400080515     C                   PARM                    tibsuni02
011500080515     C                   PARM                    rqsDataLen
011600080430     C
011700080515     C                   DOU       %EOF(ndreg00f)
011800080515     C                   READ      ndreg00f
011900080430     C
012000080430     C                   IF        %EOF()
012100080430     C                   LEAVE
012200080430     C                   ENDIF
012300080430     C
012400080515     C                   EVAL      countNdreg += 1
012500080430     C
012600080515     C                   EVAL      tibsuni01.societa = regSocieta
012700080515     C                   EVAL      tibsuni01.unita = regUnita
012800080515     C                   EVAL      rqsDataLen = %SIZE(tibsuni01)
012900080515     C                   EVAL      rpyDataLen = %SIZE(descrizioneUni)
013000080515     C                   CALL      'TIBSUNI00'
013100080515     C                   PARM      'GETDESBREV'  rqsOpCode
013200080430     C                   PARM                    rpyEsito
013300080515     C                   PARM                    tibsuni01
013400080430     C                   PARM                    rqsDataLen
013500080515     C                   PARM                    descrizioneUni
013600080515     C                   PARM                    rpyDataLen
013700080430     C
013800080430     C                   ENDDO
013900080430     C
014000080515     C                   CALL      'TIBSUNI00'
014100080430     C                   PARM      'FINALIZE'    rqsOpCode
014200080430     C                   PARM                    rpyEsito
014300080430     C
014400080515     C                   CLOSE     ndreg00f
014500080430     C                   EVAL      fine = %TIMESTAMP()
014600080430     C                   EVAL      durata = %DIFF(fine : inizio : *SECONDS)
014700080430     C                   EXCEPT    logFine
014800080509
014900080509      /FREE
015000080509
015100080509       //***********************************************************************
015200080509       //
015300080509       // Test con lettura diretta di AZORG01L.
015400080509       //
015500080509       //***********************************************************************
015600080509
015700080515       RESET countNdreg;
015800080509       inizio = %TIMESTAMP();
015900080509       EXCEPT logInizio;
016000080515       OPEN anuni01l;
016100080515       OPEN ndreg00f;
016200080515       DOU %EOF(ndreg00f);
016300080515         READ ndreg00f;
016400080509         IF %EOF();
016500080509           LEAVE;
016600080509         ENDIF;
016700080515         countNdreg += 1;
016800080515         CHAIN (regSocieta : regUnita) anuni01l;
016900080515         descrizioneUni = uniDesBrev;
017000080509       ENDDO;
017100080515       CLOSE anuni01l;
017200080515       CLOSE ndreg00f;
017300080509       fine = %TIMESTAMP();
017400080509       durata = %DIFF(fine : inizio : *SECONDS);
017500080509       EXCEPT logFine;
017600080509
017700080509      /END-FREE
017800080418
017900080429     OqSysPrt   E            logInizio   1
018000080429     O                       inizio
018100080429     O          E            logFine     1
018200080429     O                       fine
018300080515     O                                              ' NDREG00F'
018400080515     O                       countNdreg    Z
018500080429     O                                              ' Secondi'
018600080429     O                       durata        Z
018700080429
