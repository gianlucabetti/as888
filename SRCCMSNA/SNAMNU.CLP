000100900502PGM
000200000000                                      /**  DICHIARAZIONI     **/
000300000000
000400900502DCLF       FILE(*LIBL/SNAMNU0V) RCDFMT(SNAMNU01 SNAMNU02 SNALIB)
000500000000DCL &CODA *CHAR 10
000600000000DCL &JOB *CHAR 10
000700000000DCL &NUMJ *CHAR 10
000800000000DCL &DATI *CHAR 26
000900000000DCL &ID  *CHAR 7
001000000000DCL &MSG1  *CHAR 256
001100000000DCL &S *LGL
001200000000                                      /** IMPOSTA DEFAULT      **/
001300000000
001400000000             RTVJOBA    JOB(&JOB) USER(&USER) NBR(&NUMJ)
001500000000             RTVUSRPRF  MSGQ(&CODA)
001600000000
001700000000VEDI:    CHGVAR &MBR ('*FILE     ')
001800000000         CHGVAR &JOB1 ('*         ')
001900000000         CHGVAR &NUM ('*LAST ')
002000000000         CHGVAR &TIPO ('*ALL   ')
002100900608         CHGVAR &SINO ('SI')
002200000000 CHGVAR  &IDEL1 ('        ')
002300000000 CHGVAR  &IDGR1 ('        ')
002400000000 CHGVAR  &IDEL2 ('        ')
002500000000 CHGVAR  &IDGR2 ('        ')
002600000000 CHGVAR  &IDEL3 ('        ')
002700000000 CHGVAR  &IDGR3 ('        ')
002800000000 CHGVAR  &IDEL4 ('        ')
002900000000 CHGVAR  &IDGR4 ('        ')
003000000000 CHGVAR  &IDEL5 ('        ')
003100000000 CHGVAR  &IDGR5 ('        ')
003200000000 CHGVAR  &FILE  ('          ')
003300000000 CHGVAR  &LIBR  ('          ')
003400000000 CHGVAR  &LIBOBJ  ('          ')
003500900423 CHGVAR  &LIBOBJS ('          ')
003600000000
003700000000                                      /**    VISUALIZZA MENU   **/
003800000000
003900000000             SNDRCVF    RCDFMT(SNAMNU01)
004000900330IF &IN03   GOTO FINE                 /*      FINE LAVORO        */
004100900423IF &IN02 WRKSBMJOB SBMFROM(*JOB)
004200900330IF &IN04   WRKNETF
004300000000IF         COND(&IN05) THEN(WRKDSTQ)
004400900330             IF         COND(&IN06) THEN(DSPMSG MSGQ(*WRKSTN))
004500860603IF &IN07   DO
004600900330             DSPMSG     MSGQ(&CODA)
004700860613           MONMSG CPF2451    EXEC(DO)      /* coda gi� allocata    */
004800860613SNDPGMMSG  MSGID(SNA0003) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
004900860613             MSGTYPE(*STATUS)
005000860603             DLYJOB     DLY(2)
005100860603                                  ENDDO
005200860603           ENDDO
005300000000IF &IN08   DSPJOBLOG
005400860613IF &IN09   GOTO VEDI   /* � stato premuto un tasto di comando valido */
005500000000       /************************************/
005600000000       /*    INVIO FILE                    */
005700000000       /************************************/
005800000000
005900000000
006000000000IF (&SCELTA='1')  DO
006100000000FILE: CHGVAR &IN10 ('1')
006200000000      SNDRCVF *N SNAMNU02
006300900330IF &IN03   GOTO FINE       /* FINE LAVORO   */
006400900330IF &IN12   GOTO VEDI       /* MENU INIZIALE */
006500000000IF ((&MBR = '       ') *OR (&MBR = '*FILE     '))  CHGVAR &MBR &FILE
006600000000CHKOBJ     OBJ(&LIBR/&FILE) OBJTYPE(*FILE) MBR(&MBR)
006700860218             MONMSG     MSGID(CPF9800 CPF0001) EXEC(DO)
006800860613             CHGVAR  &IN11 ('1')   /* file non trovato */
006900000000             GOTO FILE
007000000000             ENDDO
007100000000
007200860613IF (&IDEL1='        ') DO         /* nessun utente scelto      */
007300000000                       CHGVAR &IN51 ('1')
007400000000                       GOTO FILE
007500000000                       ENDDO
007600860613                       IF (&IDEL1='*') DO  /*scelta lista utenti */
007700860613CALL       PGM(*LIBL/SNAUSR) PARM(&IDEL1 &IDGR1 &IDEL2 &IDGR2 &IDEL3 +
007800860613             &IDGR3 &IDEL4 &IDGR4 &IDEL5 &IDGR5)
007900000000                       GOTO FILE
008000000000                       ENDDO
008100860613/*                                                             */
008200860613/* Occorre gestire separatamente i casi di 1,2,3,4 o 5 utenti  */
008300860613/* scelti, in quanto i comandi SNDNETxxx vanno in errore se    */
008400860613/* elementi della lista TOUSRID contiene dei blanks            */
008500860613/*                                                             */
008600860613
008700000000             IF         COND(&IDEL2 = '        ') THEN(DO)
008800000000SNDNETF    FILE(&LIBR/&FILE) TOUSRID((&IDEL1 &IDGR1)) MBR(&MBR)
008900000000             MONMSG     MSGID(CPF8064 CPF8066) EXEC(DO)
009000000000             CHGVAR &IN51 ('1')
009100000000             GOTO FILE
009200000000             ENDDO
009300000000SNDPGMMSG  MSGID(SNA0001) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
009400000000             MSGTYPE(*STATUS)
009500860613             DLYJOB     DLY(2)   /* si lascia il tempo di leggere */
009600860613               GOTO AUTO         /* il messaggio                  */
009700000000               ENDDO
009800000000
009900000000             IF         COND(&IDEL3 = '        ') THEN(DO)
010000000000SNDNETF    FILE(&LIBR/&FILE) TOUSRID((&IDEL1 &IDGR1) (&IDEL2 +
010100000000             &IDGR2)) MBR(&MBR)
010200000000             MONMSG     MSGID(CPF8064 CPF8066) EXEC(DO)
010300000000             CHGVAR &IN51 ('1')
010400000000             GOTO FILE
010500000000             ENDDO
010600000000SNDPGMMSG  MSGID(SNA0001) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
010700000000             MSGTYPE(*STATUS)
010800000000             DLYJOB     DLY(2)
010900000000
011000000000               GOTO AUTO
011100000000               ENDDO
011200000000
011300000000
011400000000             IF         COND(&IDEL4 = '        ') THEN(DO)
011500000000SNDNETF    FILE(&LIBR/&FILE) TOUSRID((&IDEL1 &IDGR1) (&IDEL2 &IDGR2) +
011600000000             (&IDEL3 &IDGR3)) MBR(&MBR)
011700000000             MONMSG     MSGID(CPF8064 CPF8066) EXEC(DO)
011800000000             CHGVAR &IN51 ('1')
011900000000             GOTO FILE
012000000000             ENDDO
012100000000SNDPGMMSG  MSGID(SNA0001) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
012200000000             MSGTYPE(*STATUS)
012300000000             DLYJOB     DLY(2)
012400000000               GOTO AUTO
012500000000               ENDDO
012600000000
012700000000             IF         COND(&IDEL5 = '        ') THEN(DO)
012800000000SNDNETF    FILE(&LIBR/&FILE) TOUSRID((&IDEL1 &IDGR1) (&IDEL2 &IDGR2) +
012900000000             (&IDEL3 &IDGR3) (&IDEL4 &IDGR4)) MBR(&MBR)
013000000000             MONMSG     MSGID(CPF8064 CPF8066) EXEC(DO)
013100000000             CHGVAR &IN51 ('1')
013200000000             GOTO FILE
013300000000             ENDDO
013400000000SNDPGMMSG  MSGID(SNA0001) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
013500000000             MSGTYPE(*STATUS)
013600000000             DLYJOB     DLY(2)
013700000000               GOTO AUTO
013800000000               ENDDO
013900000000               ELSE DO
014000000000SNDNETF    FILE(&LIBR/&FILE) TOUSRID((&IDEL1 &IDGR1) (&IDEL2 &IDGR2) +
014100000000             (&IDEL3 &IDGR3) (&IDEL4 &IDGR4) (&IDEL5 &IDGR5)) +
014200000000             MBR(&MBR)
014300000000             MONMSG     MSGID(CPF8064 CPF8066) EXEC(DO)
014400000000             CHGVAR &IN51 ('1')
014500000000             GOTO FILE
014600000000             ENDDO
014700000000SNDPGMMSG  MSGID(SNA0001) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
014800000000             MSGTYPE(*STATUS)
014900000000             DLYJOB     DLY(2)
015000000000               GOTO AUTO
015100000000               ENDDO
015200000000
015300900423 AUTO:                 DO
015400000000 /****************/
015500000000 ENDDO
015600000000 CHGVAR  &FILE ('          ')
015700000000 CHGVAR &MBR ('*FILE     ')
015800000000 CHGVAR  &LIBR ('          ')
015900000000 CHGVAR  &IDEL1 ('        ')
016000000000 CHGVAR  &IDGR1 ('        ')
016100000000 CHGVAR  &IDEL2 ('        ')
016200000000 CHGVAR  &IDGR2 ('        ')
016300000000 CHGVAR  &IDEL3 ('        ')
016400000000 CHGVAR  &IDGR3 ('        ')
016500000000 CHGVAR  &IDEL4 ('        ')
016600000000 CHGVAR  &IDGR4 ('        ')
016700000000 CHGVAR  &IDEL5 ('        ')
016800000000 CHGVAR  &IDGR5 ('        ')
016900000000GOTO FILE
017000000000                  ENDDO
017100000000
017200000000
017300000000
017400000000       /************************************/
017500000000       /*    INVIO SPOOL                   */
017600000000       /************************************/
017700000000IF (&SCELTA='2')  DO
017800000000 SPOOL:      CHGVAR     VAR(&IN20) VALUE('1')
017900000000       SNDRCVF *N SNAMNU02
018000900330IF &IN03   GOTO FINE       /* FINE LAVORO   */
018100900330IF &IN12   GOTO VEDI       /* MENU INIZIALE */
018200900330IF &IN04 DO
018300000000CALL       PGM(*LIBL/SNAOUTQ)
018400900330             RTVDTAARA  DTAARA(*LDA (2 10)) RTNVAR(&FILE)
018500900330             RTVDTAARA  DTAARA(*LDA (82 10)) RTNVAR(&JOB1)
018600900330             RTVDTAARA  DTAARA(*LDA (13 10)) RTNVAR(&JOB2)
018700900330             RTVDTAARA  DTAARA(*LDA (93 6)) RTNVAR(&JOB3)
018800900330             RTVDTAARA  DTAARA(*LDA (73 4)) RTNVAR(&NUM)
018900000000         IF (&FILE='   ') GOTO SPOOL
019000000000         ELSE GOTO SNDSPL
019100000000         ENDDO
019200000000         IF (&FILE='   ') GOTO SPOOL
019300000000IF (&JOB1 *EQ '*') DO
019400000000             CHGSPLFA   FILE(&FILE) SPLNBR(&NUM)
019500860213             MONMSG     MSGID(CPF0001 CPF3303 CPF3309 CPF3343) EXEC(DO)
019600860213SNDPGMMSG  MSGID(SNA0004) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
019700860213             MSGTYPE(*STATUS)
019800860603             DLYJOB     DLY(2)
019900000000             CHGVAR &IN21 ('1')
020000000000             GOTO SPOOL
020100000000             ENDDO
020200000000             CHGVAR   &JOB1 &JOB
020300000000             CHGVAR   &JOB2 &USER
020400000000             CHGVAR   &JOB3 &NUMJ
020500000000             GOTO SNDSPL
020600000000             ENDDO
020700000000IF (&JOB2 *EQ ' ') DO
020800000000CHGSPLFA   FILE(&FILE) JOB(*N/*N/&JOB1) SPLNBR(&NUM)
020900860213             MONMSG     MSGID(CPF0001 CPF3303 CPF3309 CPF3343) EXEC(DO)
021000860213SNDPGMMSG  MSGID(SNA0004) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
021100860213             MSGTYPE(*STATUS)
021200860603             DLYJOB     DLY(2)
021300000000             CHGVAR &IN21 ('1')
021400000000             GOTO SPOOL
021500000000             ENDDO
021600000000CHGJOB     JOB(*N/*N/&JOB1) JOBQ(*LIBL/�$___�)
021700860613             /* errore creato ad arte per ricevere nel messaggio */
021800860613             /* cpf1336 il nome intero del job                   */
021900860613             MONMSG CPF0000
022000000000     RCV1:    RCVMSG     MSGDTA(&DATI) MSGID(&ID)
022100000000             IF (&ID *NE 'CPF1336') GOTO RCV1
022200000000             CHGVAR    &JOB2   %SST(&DATI 11 10)
022300000000             CHGVAR    &JOB3   %SST(&DATI 21 6)
022400000000             GOTO SNDSPL
022500000000             ENDDO
022600000000IF (&JOB3 *EQ ' ') DO
022700000000CHGSPLFA   FILE(&FILE) JOB(*N/&JOB2/&JOB1) SPLNBR(&NUM)
022800860213             MONMSG     MSGID(CPF0001 CPF3303 CPF3309 CPF3343) EXEC(DO)
022900860213SNDPGMMSG  MSGID(SNA0004) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
023000860213             MSGTYPE(*STATUS)
023100860603             DLYJOB     DLY(2)
023200000000             CHGVAR &IN21 ('1')
023300000000             GOTO SPOOL
023400000000             ENDDO
023500000000CHGJOB     JOB(*N/&JOB2/&JOB1) JOBQ(*LIBL/�$___�)
023600000000             MONMSG CPF0000
023700000000     RCV2:    RCVMSG     MSGDTA(&DATI) MSGID(&ID)
023800000000             IF (&ID *NE 'CPF1336') GOTO RCV2
023900000000             CHGVAR    &JOB3   %SST(&DATI 21 6)
024000000000             GOTO SNDSPL
024100000000             ENDDO
024200000000             ELSE DO
024300000000CHGSPLFA   FILE(&FILE) JOB(&JOB3/&JOB2/&JOB1) SPLNBR(&NUM)
024400860213             MONMSG     MSGID(CPF0001 CPF3303 CPF3309 CPF3343) EXEC(DO)
024500860213SNDPGMMSG  MSGID(SNA0004) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
024600860213             MSGTYPE(*STATUS)
024700860603             DLYJOB     DLY(2)
024800000000             CHGVAR &IN21 ('1')
024900000000             GOTO SPOOL
025000000000             ENDDO
025100000000             ENDDO
025200000000SNDSPL:      IF (&IDEL1='        ') DO
025300000000                       CHGVAR &IN51 ('1')
025400000000                       GOTO SPOOL
025500000000                       ENDDO
025600000000
025700000000                       IF (&IDEL1='*') DO
025800000000CALL       PGM(*LIBL/SNAUSR) PARM(&IDEL1 &IDGR1 &IDEL2 &IDGR2 &IDEL3 +
025900000000             &IDGR3 &IDEL4 &IDGR4 &IDEL5 &IDGR5)
026000000000                       GOTO SPOOL
026100000000                       ENDDO
026200000000
026300000000             IF         COND(&IDEL2 = '        ') THEN(DO)
026400000000SNDNETSPLF FILE(&FILE) TOUSRID((&IDEL1 &IDGR1)) +
026500000000             JOB(&JOB3/&JOB2/&JOB1) SPLNBR(&NUM)
026600000000             MONMSG     MSGID(CPF8064 CPF8066) EXEC(DO)
026700000000             CHGVAR &IN51 ('1')
026800000000             GOTO SPOOL
026900000000             ENDDO
027000000000SNDPGMMSG  MSGID(SNA0001) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
027100000000             MSGTYPE(*STATUS)
027200000000             DLYJOB     DLY(2)
027300000000               GOTO FINSPL
027400000000               ENDDO
027500000000
027600000000             IF         COND(&IDEL3 = '        ') THEN(DO)
027700000000SNDNETSPLF FILE(&FILE) TOUSRID((&IDEL1 &IDGR1) (&IDEL2 &IDGR2)) +
027800000000             JOB(&JOB3/&JOB2/&JOB1) SPLNBR(&NUM)
027900000000             MONMSG     MSGID(CPF8064 CPF8066) EXEC(DO)
028000000000             CHGVAR &IN51 ('1')
028100000000             GOTO SPOOL
028200000000             ENDDO
028300000000SNDPGMMSG  MSGID(SNA0001) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
028400000000             MSGTYPE(*STATUS)
028500000000             DLYJOB     DLY(2)
028600000000               GOTO FINSPL
028700000000               ENDDO
028800000000
028900000000
029000000000             IF         COND(&IDEL4 = '        ') THEN(DO)
029100000000SNDNETSPLF FILE(&FILE) TOUSRID((&IDEL1 &IDGR1) (&IDEL2 &IDGR2) +
029200000000             (&IDEL3 &IDGR3)) JOB(&JOB3/&JOB2/&JOB1) SPLNBR(&NUM)
029300000000             MONMSG     MSGID(CPF8064 CPF8066) EXEC(DO)
029400000000             CHGVAR &IN51 ('1')
029500000000             GOTO SPOOL
029600000000             ENDDO
029700000000SNDPGMMSG  MSGID(SNA0001) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
029800000000             MSGTYPE(*STATUS)
029900000000             DLYJOB     DLY(2)
030000000000               GOTO FINSPL
030100000000               ENDDO
030200000000
030300000000             IF         COND(&IDEL5 = '        ') THEN(DO)
030400000000SNDNETSPLF FILE(&FILE) TOUSRID((&IDEL1 &IDGR1) (&IDEL2 &IDGR2) +
030500000000             (&IDEL3 &IDGR3) (&IDEL4 &IDGR4)) JOB(&JOB3/&JOB2/&JOB1) +
030600000000             SPLNBR(&NUM)
030700000000             MONMSG     MSGID(CPF8064 CPF8066) EXEC(DO)
030800000000             CHGVAR &IN51 ('1')
030900000000             GOTO SPOOL
031000000000             ENDDO
031100000000SNDPGMMSG  MSGID(SNA0001) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
031200000000             MSGTYPE(*STATUS)
031300000000             DLYJOB     DLY(2)
031400000000               GOTO FINSPL
031500000000             ENDDO
031600000000            ELSE DO
031700000000SNDNETSPLF FILE(&FILE) TOUSRID((&IDEL1 &IDGR1) (&IDEL2 &IDGR2) +
031800000000             (&IDEL3 &IDGR3) (&IDEL4 &IDGR4) (&IDEL5 &IDGR5)) +
031900000000             JOB(&JOB3/&JOB2/&JOB1) SPLNBR(&NUM)
032000000000             MONMSG     MSGID(CPF8064 CPF8066) EXEC(DO)
032100000000             CHGVAR &IN51 ('1')
032200000000              GOTO SPOOL
032300000000                  ENDDO
032400000000SNDPGMMSG  MSGID(SNA0001) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
032500000000             MSGTYPE(*STATUS)
032600000000             DLYJOB     DLY(2)
032700000000                  ENDDO
032800000000 FINSPL:   CHGVAR  &FILE ('          ')
032900000000 CHGVAR  &JOB1 ('*         ')
033000000000 CHGVAR  &JOB2 ('          ')
033100000000 CHGVAR  &JOB3 ('          ')
033200000000CHGVAR &NUM ('*LAST ')
033300000000 CHGVAR  &IDEL1 ('        ')
033400000000 CHGVAR  &IDGR1 ('        ')
033500000000 CHGVAR  &IDEL2 ('        ')
033600000000 CHGVAR  &IDGR2 ('        ')
033700000000 CHGVAR  &IDEL3 ('        ')
033800000000 CHGVAR  &IDGR3 ('        ')
033900000000 CHGVAR  &IDEL4 ('        ')
034000000000 CHGVAR  &IDGR4 ('        ')
034100000000 CHGVAR  &IDEL5 ('        ')
034200000000 CHGVAR  &IDGR5 ('        ')
034300000000              GOTO SPOOL
034400000000ENDDO
034500000000       /************************************/
034600000000       /*    INVIO MESSAGGI                */
034700000000       /************************************/
034800000000IF (&SCELTA='3')  DO
034900000000 MSG:        CHGVAR     VAR(&IN30) VALUE('1')
035000000000      SNDRCVF *N SNAMNU02
035100900330IF &IN03   GOTO FINE       /* FINE LAVORO   */
035200900330IF &IN12   GOTO VEDI       /* MENU INIZIALE */
035300000000IF (&MSG = '    ') GOTO MSG
035400000000             IF (&IDEL1='        ') DO
035500000000                       CHGVAR &IN51 ('1')
035600000000                       GOTO MSG
035700000000                       ENDDO
035800000000
035900000000                       IF (&IDEL1='*') DO
036000000000CALL       PGM(*LIBL/SNAUSR) PARM(&IDEL1 &IDGR1 &IDEL2 &IDGR2 &IDEL3 +
036100000000             &IDGR3 &IDEL4 &IDGR4 &IDEL5 &IDGR5)
036200000000                       GOTO MSG
036300000000                       ENDDO
036400000000
036500000000             IF         COND(&IDEL2 = '        ') THEN(DO)
036600000000             SNDNETMSG  MSG(&MSG) TOUSRID((&IDEL1 &IDGR1))
036700000000             MONMSG     MSGID(CPF8064 CPF8066) EXEC(DO)
036800000000             CHGVAR &IN51 ('1')
036900000000             GOTO MSG
037000000000             ENDDO
037100000000SNDPGMMSG  MSGID(SNA0001) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
037200000000             MSGTYPE(*STATUS)
037300000000             DLYJOB     DLY(2)
037400000000               GOTO FINMSG
037500000000               ENDDO
037600000000
037700000000             IF         COND(&IDEL3 = '        ') THEN(DO)
037800000000             SNDNETMSG  MSG(&MSG) TOUSRID((&IDEL1 &IDGR1) (&IDEL2 +
037900000000                          &IDGR2))
038000000000             MONMSG     MSGID(CPF8064 CPF8066) EXEC(DO)
038100000000             CHGVAR &IN51 ('1')
038200000000             GOTO MSG
038300000000             ENDDO
038400000000SNDPGMMSG  MSGID(SNA0001) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
038500000000             MSGTYPE(*STATUS)
038600000000             DLYJOB     DLY(2)
038700000000               GOTO FINMSG
038800000000               ENDDO
038900000000
039000000000
039100000000             IF         COND(&IDEL4 = '        ') THEN(DO)
039200000000             SNDNETMSG  MSG(&MSG) TOUSRID((&IDEL1 &IDGR1) (&IDEL2 +
039300000000                          &IDGR2) (&IDEL3 &IDGR3))
039400000000             MONMSG     MSGID(CPF8064 CPF8066) EXEC(DO)
039500000000             CHGVAR &IN51 ('1')
039600000000             GOTO MSG
039700000000             ENDDO
039800000000SNDPGMMSG  MSGID(SNA0001) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
039900000000             MSGTYPE(*STATUS)
040000000000             DLYJOB     DLY(2)
040100000000               GOTO FINMSG
040200000000               ENDDO
040300000000
040400000000             IF         COND(&IDEL5 = '        ') THEN(DO)
040500000000             SNDNETMSG  MSG(&MSG) TOUSRID((&IDEL1 &IDGR1) (&IDEL2 +
040600000000                          &IDGR2) (&IDEL3 &IDGR3)(&IDEL4 &IDGR4))
040700000000             MONMSG     MSGID(CPF8064 CPF8066) EXEC(DO)
040800000000             CHGVAR &IN51 ('1')
040900000000             GOTO MSG
041000000000             ENDDO
041100000000SNDPGMMSG  MSGID(SNA0001) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
041200000000             MSGTYPE(*STATUS)
041300000000             DLYJOB     DLY(2)
041400000000               GOTO FINMSG
041500000000             ENDDO
041600000000            ELSE DO
041700000000             SNDNETMSG  MSG(&MSG) TOUSRID((&IDEL1 &IDGR1) (&IDEL2 +
041800000000                          &IDGR2) (&IDEL3 &IDGR3) (&IDEL4 &IDGR4) +
041900000000                          (&IDEL5 &IDGR5))
042000000000             MONMSG     MSGID(CPF8064 CPF8066) EXEC(DO)
042100000000             CHGVAR &IN51 ('1')
042200000000              GOTO MSG
042300000000                  ENDDO
042400000000SNDPGMMSG  MSGID(SNA0001) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
042500000000             MSGTYPE(*STATUS)
042600000000             DLYJOB     DLY(2)
042700000000                  ENDDO
042800000000 FINMSG:   CHGVAR  &MSG  &MSG1
042900000000 CHGVAR  &IDEL1 ('        ')
043000000000 CHGVAR  &IDGR1 ('        ')
043100000000 CHGVAR  &IDEL2 ('        ')
043200000000 CHGVAR  &IDGR2 ('        ')
043300000000 CHGVAR  &IDEL3 ('        ')
043400000000 CHGVAR  &IDGR3 ('        ')
043500000000 CHGVAR  &IDEL4 ('        ')
043600000000 CHGVAR  &IDGR4 ('        ')
043700000000 CHGVAR  &IDEL5 ('        ')
043800000000 CHGVAR  &IDGR5 ('        ')
043900000000              GOTO MSG
044000000000ENDDO
044100000000
044200000000
044300000000       /************************************/
044400000000       /*    INVIO ALTRI OGGETTI           */
044500000000       /************************************/
044600000000
044700000000
044800000000IF (&SCELTA='4')  DO
044900000000 OBJ:        CHGVAR     VAR(&IN40) VALUE('1')
045000000000     SNDRCVF *N SNAMNU02
045100900330IF &IN03   GOTO FINE       /* FINE LAVORO   */
045200900330IF &IN12   GOTO VEDI       /* MENU INIZIALE */
045300000000IF &S   GOTO   CTL
045400000000IF ((&OBJ=' ') *OR (&LIBOBJ=' ')) DO
045500000000                        CHGVAR &IN41 ('1')
045600000000                        GOTO OBJ
045700000000                        ENDDO
045800000000CRTSAVF    FILE(QTEMP/&LIBOBJ)
045900000000MONMSG CPF0000
046000000000SNDPGMMSG  MSGID(SNA0002) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
046100000000             MSGTYPE(*STATUS)
046200900330             SAVOBJ     OBJ(&OBJ) LIB(&LIBOBJ) DEV(*SAVF) +
046300900330                          OBJTYPE(&TIPO) SAVF(QTEMP/&LIBOBJ) +
046400900330                          CLEAR(*ALL) PRECHK(*YES) DTACPR(*YES)
046500000000             MONMSG     MSGID(CPF0001 CPF3749 CPF3781) EXEC(DO)
046600000000                        CHGVAR &IN41 ('1')
046700000000                        GOTO OBJ
046800000000                        ENDDO
046900860613CHGVAR  &S    ('1')      /* salvataggio gi� effettuato  */
047000000000CTL:   IF (&IDEL1='        ') DO
047100000000                       CHGVAR &IN51 ('1')
047200000000                       GOTO OBJ
047300000000                       ENDDO
047400000000
047500000000                       IF (&IDEL1='*') DO
047600000000CALL       PGM(*LIBL/SNAUSR) PARM(&IDEL1 &IDGR1 &IDEL2 &IDGR2 &IDEL3 +
047700000000             &IDGR3 &IDEL4 &IDGR4 &IDEL5 &IDGR5)
047800000000                       GOTO OBJ
047900000000                       ENDDO
048000000000
048100000000             IF         COND(&IDEL2 = '        ') THEN(DO)
048200000000SNDNETF    FILE(QTEMP/&LIBOBJ) TOUSRID((&IDEL1 &IDGR1))
048300000000             MONMSG     MSGID(CPF8064 CPF8066) EXEC(DO)
048400000000             CHGVAR &IN51 ('1')
048500000000             GOTO OBJ
048600000000             ENDDO
048700000000SNDPGMMSG  MSGID(SNA0001) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
048800000000             MSGTYPE(*STATUS)
048900000000             DLYJOB     DLY(2)
049000000000               GOTO AUTOBJ
049100000000               ENDDO
049200000000
049300000000             IF         COND(&IDEL3 = '        ') THEN(DO)
049400000000SNDNETF    FILE(QTEMP/&LIBOBJ) TOUSRID((&IDEL1 &IDGR1) (&IDEL2 +
049500000000             &IDGR2))
049600000000             MONMSG     MSGID(CPF8064 CPF8066) EXEC(DO)
049700000000             CHGVAR &IN51 ('1')
049800000000             GOTO OBJ
049900000000             ENDDO
050000000000SNDPGMMSG  MSGID(SNA0001) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
050100000000             MSGTYPE(*STATUS)
050200000000             DLYJOB     DLY(2)
050300000000               GOTO AUTOBJ
050400000000               ENDDO
050500000000
050600000000
050700000000             IF         COND(&IDEL4 = '        ') THEN(DO)
050800000000SNDNETF    FILE(QTEMP/&LIBOBJ) TOUSRID((&IDEL1 &IDGR1) (&IDEL2 +
050900000000             &IDGR2) (&IDEL3 &IDGR3))
051000000000             MONMSG     MSGID(CPF8064 CPF8066) EXEC(DO)
051100000000             CHGVAR &IN51 ('1')
051200000000             GOTO OBJ
051300000000             ENDDO
051400000000SNDPGMMSG  MSGID(SNA0001) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
051500000000             MSGTYPE(*STATUS)
051600000000             DLYJOB     DLY(2)
051700000000               GOTO AUTOBJ
051800000000               ENDDO
051900000000
052000000000             IF         COND(&IDEL5 = '        ') THEN(DO)
052100000000SNDNETF    FILE(QTEMP/&LIBOBJ) TOUSRID((&IDEL1 &IDGR1) (&IDEL2 +
052200000000             &IDGR2) (&IDEL3 &IDGR3) (&IDEL4 &IDGR4))
052300000000             MONMSG     MSGID(CPF8064 CPF8066) EXEC(DO)
052400000000             CHGVAR &IN51 ('1')
052500000000             GOTO OBJ
052600000000             ENDDO
052700000000SNDPGMMSG  MSGID(SNA0001) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
052800000000             MSGTYPE(*STATUS)
052900000000             DLYJOB     DLY(2)
053000000000               GOTO AUTOBJ
053100000000               ENDDO
053200000000               ELSE DO
053300000000SNDNETF    FILE(QTEMP/&LIBOBJ) TOUSRID((&IDEL1 &IDGR1) (&IDEL2 +
053400000000             &IDGR2) (&IDEL3 &IDGR3) (&IDEL4 &IDGR4) (&IDEL5 &IDGR5))
053500000000             MONMSG     MSGID(CPF8064 CPF8066) EXEC(DO)
053600000000             CHGVAR &IN51 ('1')
053700000000             GOTO OBJ
053800000000             ENDDO
053900000000SNDPGMMSG  MSGID(SNA0001) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
054000000000             MSGTYPE(*STATUS)
054100000000             DLYJOB     DLY(2)
054200000000               GOTO AUTOBJ
054300000000               ENDDO
054400000000 AUTOBJ: CHGVAR &S ('0')
054500000000 /****************/
054600000000 CHGVAR  &OBJ  ('          ')
054700000000 CHGVAR  &TIPO ('*ALL   ')
054800000000 CHGVAR  &LIBOBJ ('          ')
054900000000 CHGVAR  &IDEL1 ('        ')
055000000000 CHGVAR  &IDGR1 ('        ')
055100000000 CHGVAR  &IDEL2 ('        ')
055200000000 CHGVAR  &IDGR2 ('        ')
055300000000 CHGVAR  &IDEL3 ('        ')
055400000000 CHGVAR  &IDGR3 ('        ')
055500000000 CHGVAR  &IDEL4 ('        ')
055600000000 CHGVAR  &IDGR4 ('        ')
055700000000 CHGVAR  &IDEL5 ('        ')
055800000000 CHGVAR  &IDGR5 ('        ')
055900000000
056000000000GOTO OBJ
056100000000                  ENDDO
056200000000
056300900423
056400900423       /************************************/
056500900423       /*    INVIO LIBRERIE                */
056600900423       /************************************/
056700900423
056800900423IF (&SCELTA = '5')  DO
056900900423 LIB:        CHGVAR     VAR(&IN60) VALUE('1')
057000900423     SNDRCVF *N SNAMNU02
057100900423IF &IN03   GOTO FINE       /* FINE LAVORO   */
057200900423IF &IN12   GOTO VEDI       /* MENU INIZIALE */
057300900502IF &S GOTO CTLL
057400900423CHKOBJ   QSYS/&LIBOBJS *LIB
057500900423MONMSG CPF9801   EXEC(DO)
057600900423                        CHGVAR &IN61 ('1')
057700900423                        GOTO LIB
057800900423                        ENDDO
057900900423IF (&LIBOBJS=' ')  DO
058000900423                        CHGVAR &IN61 ('1')
058100900423                        GOTO LIB
058200900423                        ENDDO
058300900423CRTSAVF    FILE(QTEMP/&LIBOBJS)
058400900423MONMSG CPF0000
058500900423SNDPGMMSG  MSGID(SNA0002) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
058600900423             MSGTYPE(*STATUS)
058700900423             SAVLIB     LIB(&LIBOBJS) DEV(*SAVF) SAVF(QTEMP/&LIBOBJS) +
058800900423                          CLEAR(*ALL) PRECHK(*YES) DTACPR(*YES)
058900900423             MONMSG     MSGID(CPF0001 CPF3749 CPF3781) EXEC(DO)
059000900423                        CHGVAR &IN41 ('1')
059100900502                        GOTO LIB
059200900423                        ENDDO
059300900423CHGVAR  &S    ('1')      /* salvataggio gi� effettuato  */
059400900423CTLL:   IF (&IDEL1='        ') DO
059500900423                       CHGVAR &IN51 ('1')
059600900502                       GOTO LIB
059700900423                       ENDDO
059800900423
059900900423                       IF (&IDEL1='*') DO
060000900423CALL       PGM(*LIBL/SNAUSR) PARM(&IDEL1 &IDGR1 &IDEL2 &IDGR2 &IDEL3 +
060100900423             &IDGR3 &IDEL4 &IDGR4 &IDEL5 &IDGR5)
060200900502                       GOTO LIB
060300900423                       ENDDO
060400900423
060500900423             IF         COND(&IDEL2 = '        ') THEN(DO)
060600900423             SNDNETF    FILE(QTEMP/&LIBOBJS) TOUSRID((&IDEL1 &IDGR1))
060700900423             MONMSG     MSGID(CPF8064 CPF8066) EXEC(DO)
060800900423             CHGVAR &IN51 ('1')
060900900502             GOTO LIB
061000900423             ENDDO
061100900423SNDPGMMSG  MSGID(SNA0001) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
061200900423             MSGTYPE(*STATUS)
061300900423             DLYJOB     DLY(2)
061400900423               GOTO FINLIB
061500900423               ENDDO
061600900423
061700900423             IF         COND(&IDEL3 = '        ') THEN(DO)
061800900423             SNDNETF    FILE(QTEMP/&LIBOBJS) TOUSRID((&IDEL1 &IDGR1) +
061900900423                          (&IDEL2 &IDGR2))
062000900423             MONMSG     MSGID(CPF8064 CPF8066) EXEC(DO)
062100900423             CHGVAR &IN51 ('1')
062200900502             GOTO LIB
062300900423             ENDDO
062400900423SNDPGMMSG  MSGID(SNA0001) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
062500900423             MSGTYPE(*STATUS)
062600900423             DLYJOB     DLY(2)
062700900423               GOTO FINLIB
062800900423               ENDDO
062900900423
063000900423
063100900423             IF         COND(&IDEL4 = '        ') THEN(DO)
063200900423             SNDNETF    FILE(QTEMP/&LIBOBJS) TOUSRID((&IDEL1 &IDGR1) +
063300900423                          (&IDEL2 &IDGR2) (&IDEL3 &IDGR3))
063400900423             MONMSG     MSGID(CPF8064 CPF8066) EXEC(DO)
063500900423             CHGVAR &IN51 ('1')
063600900502             GOTO LIB
063700900423             ENDDO
063800900423SNDPGMMSG  MSGID(SNA0001) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
063900900423             MSGTYPE(*STATUS)
064000900423             DLYJOB     DLY(2)
064100900423               GOTO FINLIB
064200900423               ENDDO
064300900423
064400900423             IF         COND(&IDEL5 = '        ') THEN(DO)
064500900423             SNDNETF    FILE(QTEMP/&LIBOBJS) TOUSRID((&IDEL1 &IDGR1) +
064600900423                          (&IDEL2 &IDGR2) (&IDEL3 &IDGR3) (&IDEL4 +
064700900423                          &IDGR4))
064800900423             MONMSG     MSGID(CPF8064 CPF8066) EXEC(DO)
064900900423             CHGVAR &IN51 ('1')
065000900502             GOTO LIB
065100900423             ENDDO
065200900423SNDPGMMSG  MSGID(SNA0001) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
065300900423             MSGTYPE(*STATUS)
065400900423             DLYJOB     DLY(2)
065500900423               GOTO FINLIB
065600900423               ENDDO
065700900423               ELSE DO
065800900423             SNDNETF    FILE(QTEMP/&LIBOBJS) TOUSRID((&IDEL1 &IDGR1) +
065900900423                          (&IDEL2 &IDGR2) (&IDEL3 &IDGR3) (&IDEL4 +
066000900423                          &IDGR4) (&IDEL5 &IDGR5))
066100900423             MONMSG     MSGID(CPF8064 CPF8066) EXEC(DO)
066200900423             CHGVAR &IN51 ('1')
066300900502             GOTO LIB
066400900423             ENDDO
066500900423SNDPGMMSG  MSGID(SNA0001) MSGF(*LIBL/SNAMSG) TOPGMQ(*EXT) +
066600900423             MSGTYPE(*STATUS)
066700900423             DLYJOB     DLY(2)
066800900423               GOTO FINLIB
066900900423               ENDDO
067000900423 FINLIB: CHGVAR &S ('0')
067100900423 /****************/
067200900423 CHGVAR  &OBJ  ('          ')
067300900423 CHGVAR  &TIPO ('*ALL   ')
067400900423 CHGVAR  &LIBOBJS ('          ')
067500900423 CHGVAR  &IDEL1 ('        ')
067600900423 CHGVAR  &IDGR1 ('        ')
067700900423 CHGVAR  &IDEL2 ('        ')
067800900423 CHGVAR  &IDGR2 ('        ')
067900900423 CHGVAR  &IDEL3 ('        ')
068000900423 CHGVAR  &IDGR3 ('        ')
068100900423 CHGVAR  &IDEL4 ('        ')
068200900423 CHGVAR  &IDGR4 ('        ')
068300900423 CHGVAR  &IDEL5 ('        ')
068400900423 CHGVAR  &IDGR5 ('        ')
068500900423
068600900423GOTO LIB
068700900423                     ENDDO
068800900705       /************************************/
068900900705       /*    RICEZIONE OGGETTI             */
069000900705       /************************************/
069100900705
069200900705IF (&SCELTA = '6')  DO
069300900705             WRKNETF    OUTPUT(*OUTFILE) OUTFILE(QTEMP/NETFL00F)
069400900705             OVRDBF  NETFL00F   QTEMP/NETFL00F
069500900705             CALL SNANETFR
069600900705             GOTO  VEDI
069700900705                     ENDDO
069800900423
069900900423       /************************************/
070000900423       /*    INVIO NO SNADS                */
070100900423       /************************************/
070200900423
070300900423IF (&SCELTA = '9')  DO
070400900423 SAV:        CHGVAR     VAR(&IN70) VALUE('1')
070500900502   SNDRCVF *N SNALIB
070600900502IF &IN03   GOTO FINE       /* FINE LAVORO   */
070700900502IF &IN12   GOTO VEDI       /* MENU INIZIALE */
070800900502IF &IN04   DSPAPPNINF
070900900608IF &IN04   GOTO SAV
071000900502IF (&IN75 *AND &IN76) DO
071100900502                      CHGVAR &IN71 ('1')
071200900502                      CHGVAR &IN72 ('1')
071300900502                      GOTO SAV
071400900502                      ENDDO
071500900608IF &IN75     DO
071600900502CHKOBJ   QSYS/&LIB$    *LIB
071700900502MONMSG CPF9801   EXEC(DO)
071800900502                        CHGVAR &IN71 ('1')
071900900502                        GOTO SAV
072000900502                        ENDDO
072100900608             ENDDO
072200900608IF &IN76     DO
072300900608CHKOBJ   QSYS/&LIB$$   *LIB
072400900608MONMSG CPF9801   EXEC(DO)
072500900608                        CHGVAR &IN72 ('1')
072600900608                        GOTO SAV
072700900608                        ENDDO
072800900608             ENDDO
072900900516             CHGDTAARA  DTAARA(*LDA) VALUE('                         -
073000900516                                                                     -
073100900516                                                                     -
073200900516                                                                     -
073300900516                                                             ')
073400900608IF (&LIBAR$='  ' *AND &IN75) CHGVAR &LIBAR$  &LIB$
073500900608IF (&LIBAR$='  ' *AND &IN76) CHGVAR &LIBAR$  &LIB$$
073600900516IF  &IN76  CHGDTAARA       (*LDA (1 1))    ('O')
073700900516ELSE       CHGDTAARA       (*LDA (1 1))    ('L')
073800900516CHGDTAARA   (*LDA (11 10))     &LIB$
073900900516CHGDTAARA   (*LDA (21 10))     &OBJ$
074000900516CHGDTAARA   (*LDA (31 10))     &LIB$$
074100900516CHGDTAARA   (*LDA (41 10))     &LIBAR$
074200900516CHGDTAARA   (*LDA (51 10))     &SYS$
074300900516CHGDTAARA   (*LDA (61 7))      &TIPO
074400900608CHGDTAARA   (*LDA (71 2))      &SINO
074500900516SBMJOB  JOB(SNDSAV)   CMD(CALL SNASAV)
074600900423GOTO SAV
074700900423                     ENDDO
074800000000GOTO VEDI
074900000000FINE: ENDPGM
