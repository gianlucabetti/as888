000100080430     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('TIBS')
000200080418
000300080418     Fazorg01l  IF   E           K DISK    USROPN
000400080418     Ftitas00f  IF   E             DISK    USROPN
000500080418     FqSysPrt   O    F  132        PRINTER
000600080418
000700080430     ***************************************************************************
000800080430     **
000900080430     ** Definizione prototipi procedure.
001000080430     **
001100080430     ***************************************************************************
001200080430     D/COPY GAITRASRC/SRCPROTOPR,TIBSORG00
001300080430
001400080429     ***************************************************************************
001500080429     **
001600080429     ** Definizione costanti.
001700080429     **
001800080429     ***************************************************************************
001900080430     D ORGANIGRAMMA_ESITO_ERRORE_GENERICO...
002000080429     D                 C                   1
002100080430     D ORGANIGRAMMA_ESITO_ERRORE_FILIALE_CODICENONVALIDO...
002200080429     D                 C                   2
002300080430     D ORGANIGRAMMA_ESITO_ERRORE_FILIALE_NONESISTE...
002400080429     D                 C                   3
002500080430     D ORGANIGRAMMA_ESITO_ERRORE_OPCODE_SCONOSCIUTO...
002600080429     D                 C                   4
002700080429     D ORGANIGRAMMA_OPCODE_GETANAGRAFICAFILIALE...
002800080429     D                 C                   'GETANAGFIL'
002900080429     D ORGANIGRAMMA_OPCODE_INIT...
003000080429     D                 C                   'INIT'
003100080429     D ORGANIGRAMMA_OPCODE_ISINIT...
003200080429     D                 C                   'ISINIT'
003300080429     D ORGANIGRAMMA_OPCODE_FINALIZE...
003400080429     D                 C                   'FINALIZE'
003500080429
003600080429     ***************************************************************************
003700080429     **
003800080429     ** Definizione strutture dati.
003900080429     **
004000080429     ***************************************************************************
004100080509     D azorg00F      E DS                  QUALIFIED BASED(nullPtr)
004101140326     D kpjba         E DS                  QUALIFIED BASED(nullPtr)
004200080430     D tibsorg01     E DS                  QUALIFIED
004300080430
004400080429     ***************************************************************************
004500080429     **
004600080429     ** Definizione variabili globali.
004700080429     **
004800080429     ***************************************************************************
004900080509     D descrizioneLnp  S                   LIKE(azorg00f.orgDes)
005000080509     D descrizioneLna  S                   LIKE(azorg00f.orgDes)
005100080429     D inizio          S               Z
005200080429     D fine            S               Z
005300080429     D countTitas      S             10U 0
005400080429     D durata          S              5U 0
005500080429     D rqsOpCode       S             10A
005600080429     D rpyEsito        S             10I 0
005700080429     D rqsData         S              2A
005800080429     D rqsDataLen      S             10I 0
005900080429     D rpyData         S           4282A
006000080429     D rpyDataLen      S             10I 0
006001140326     D indirizzoEmailInfo...
006002140326     D                 S            255A   VARYING
006100080429
006200080429      /FREE
006300080429
006400080429       *INLR = *ON;
006500080429
006600080430       //***********************************************************************
006700080430       //
006800080430       // Test con chiamata al modulo.
006900080430       //
007000080430       //***********************************************************************
007100080430
007200080429       RESET countTitas;
007300080429       inizio = %TIMESTAMP();
007400080429       EXCEPT logInizio;
007500080429       OPEN titas00f;
007600080429       IF NOT Organigramma_isInit();
007700080429         Organigramma_init();
007800080429       ENDIF;
007900080429       DOU %EOF(titas00f);
008000080429         READ titas00f;
008100080429         IF %EOF();
008200080429           LEAVE;
008300080429         ENDIF;
008400080429         countTitas += 1;
008500080509         Organigramma_getDescrizioneFiliale(tasLnp : descrizioneLnp);
008501140326         Organigramma_getRecapitiFiliale( tasLnp
008502140326                                        : *OMIT
008503140326                                        : *OMIT
008504140326                                        : *OMIT
008505140326                                        : indirizzoEmailInfo
008506140326                                        );
008600080509         Organigramma_getDescrizioneFiliale(tasLna : descrizioneLna);
008700080429       ENDDO;
008800080429       Organigramma_finalize();
008900080429       CLOSE titas00f;
009000080429       fine = %TIMESTAMP();
009100080429       durata = %DIFF(fine : inizio : *SECONDS);
009200080429       EXCEPT logFine;
009300080430
009400080430      /END-FREE
009500080430
009600080430     ***************************************************************************
009700080430     **
009800080430     ** Test con chiamata al programma.
009900080430     **
010000080430     ***************************************************************************
010100080430
010200080430     C                   RESET                   countTitas
010300080430     C                   EVAL      inizio = %TIMESTAMP()
010400080430     C                   EXCEPT    logInizio
010500080430     C                   OPEN      titas00f
010600080430     C
010700080430     C                   CALL      'TIBSORG00'
010800080430     C                   PARM      'INIT'        rqsOpCode
010900080430     C                   PARM                    rpyEsito
011000080430     C
011100080430     C                   DOU       %EOF(titas00f)
011200080430     C                   READ      titas00f
011300080430     C
011400080430     C                   IF        %EOF()
011500080430     C                   LEAVE
011600080430     C                   ENDIF
011700080430     C
011800080430     C                   EVAL      countTitas += 1
011900080430     C
012000080430     C                   EVAL      tibsorg01.fil = tasLnp
012100080430     C                   EVAL      rqsDataLen = %SIZE(tibsorg01)
012200080509     C                   EVAL      rpyDataLen = %SIZE(descrizioneLnp)
012300080430     C                   CALL      'TIBSORG00'
012400080509     C                   PARM      'GETDESFIL'   rqsOpCode
012500080430     C                   PARM                    rpyEsito
012600080515     C                   PARM                    tibsorg01
012700080430     C                   PARM                    rqsDataLen
012800080515     C                   PARM                    descrizioneLnp
012900080515     C                   PARM                    rpyDataLen
013000080430     C
013100080430     C                   EVAL      tibsorg01.fil = tasLna
013200080430     C                   EVAL      rqsDataLen = %SIZE(tibsorg01)
013300080509     C                   EVAL      rpyDataLen = %SIZE(descrizioneLna)
013400080430     C                   CALL      'TIBSORG00'
013500080509     C                   PARM      'GETDESFIL'   rqsOpCode
013600080430     C                   PARM                    rpyEsito
013700080515     C                   PARM                    tibsorg01
013800080430     C                   PARM                    rqsDataLen
013900080515     C                   PARM                    descrizioneLna
014000080515     C                   PARM                    rpyDataLen
014100080430     C
014200080430     C                   ENDDO
014300080430     C
014400080430     C                   CALL      'TIBSORG00'
014500080430     C                   PARM      'FINALIZE'    rqsOpCode
014600080430     C                   PARM                    rpyEsito
014700080430     C
014800080430     C                   CLOSE     titas00f
014900080430     C                   EVAL      fine = %TIMESTAMP()
015000080430     C                   EVAL      durata = %DIFF(fine : inizio : *SECONDS)
015100080430     C                   EXCEPT    logFine
015200080509
015300080509      /FREE
015400080509
015500080509       //***********************************************************************
015600080509       //
015700080509       // Test con lettura diretta di AZORG01L.
015800080509       //
015900080509       //***********************************************************************
016000080509
016100080509       RESET countTitas;
016200080509       inizio = %TIMESTAMP();
016300080509       EXCEPT logInizio;
016400080509       OPEN azorg01l;
016500080509       OPEN titas00f;
016600080509       DOU %EOF(titas00f);
016700080509         READ titas00f;
016800080509         IF %EOF();
016900080509           LEAVE;
017000080509         ENDIF;
017100080509         countTitas += 1;
017200080509         CHAIN tasLnp azorg01l;
017300080509         descrizioneLnp = orgDes;
017400080509         CHAIN tasLna azorg01l;
017500080509         descrizioneLna = orgDes;
017600080509       ENDDO;
017700080509       CLOSE azorg01l;
017800080509       CLOSE titas00f;
017900080509       fine = %TIMESTAMP();
018000080509       durata = %DIFF(fine : inizio : *SECONDS);
018100080509       EXCEPT logFine;
018200090107
018300090107       //***********************************************************************
018400090107       //
018500090107       // Test con lettura SQL diretta di AZORG01L.
018600090107       //
018700090107       //***********************************************************************
018800090107
018900090107       RESET countTitas;
019000090107       inizio = %TIMESTAMP();
019100090107       EXCEPT logInizio;
019200090107       OPEN titas00f;
019300090107       DOU %EOF(titas00f);
019400090107         READ titas00f;
019500090107         IF %EOF();
019600090107           LEAVE;
019700090107         ENDIF;
019800090107         countTitas += 1;
019900090107         EXEC SQL
020000090107           SELECT ORGDES
020100090107             INTO :descrizioneLnp
020200090107             FROM AZORG00F
020300090107             WHERE ORGFIL = :tasLnp
020400090107         ;
020500090107         EXEC SQL
020600090107           SELECT ORGDES
020700090107             INTO :descrizioneLna
020800090107             FROM AZORG00F
020900090107             WHERE ORGFIL = :tasLna
021000090107         ;
021100090107       ENDDO;
021200090107       CLOSE titas00f;
021300090107       fine = %TIMESTAMP();
021400090107       durata = %DIFF(fine : inizio : *SECONDS);
021500090107       EXCEPT logFine;
021600080509
021700080509      /END-FREE
021800080418
021900080429     OqSysPrt   E            logInizio   1
022000080429     O                       inizio
022100080429     O          E            logFine     1
022200080429     O                       fine
022300080429     O                                              ' TITAS00F'
022400080429     O                       countTitas    Z
022500080429     O                                              ' Secondi'
022600080429     O                       durata        Z
022700080429
