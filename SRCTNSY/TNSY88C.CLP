000100011020 TNSY88C:    PGM
000200921127             DCL        VAR(&RISP) TYPE(*CHAR) LEN(2)
000300930903             DCL        VAR(&USER) TYPE(*CHAR) LEN(10)
000400950210             DCL        VAR(&TUTE) TYPE(*CHAR) LEN(3)
000500950210             DCL        VAR(&TDSP) TYPE(*CHAR) LEN(3)
000600970324             DCL        VAR(&TDSPF) TYPE(*CHAR) LEN(6)
000700930902             DCL        VAR(&DSP) TYPE(*CHAR) LEN(10)
000800990706             GOTO       CMDLBL(NOTEST)
000900930903             RTVJOBA    JOB(&DSP) USER(&USER)
001000950210             CHGVAR     VAR(&TUTE) VALUE(%SST(&USER 1 3))
001100950210             CHGVAR     VAR(&TDSP) VALUE(%SST(&DSP 1 3))
001200970324             CHGVAR     VAR(&TDSPF) VALUE(%SST(&DSP 1 6))
001300950210
001400950210             /* APERTURA CON STESSO PROFILO */
001500950210
001600990406             IF         COND((&TUTE *NE 'EDP') *AND (&TUTE *NE +
001700990406                          'ISP') *AND (&TUTE *NE 'DIV') *AND (&TUTE +
001800990406                          *NE 'OPR') *AND (&TUTE *NE 'INT') *AND +
001900990406                          (&USER *NE 'SEG001') *AND (&USER *NE +
002000990406                          'ASC99')) THEN(DO)
002100930903             ALCOBJ     OBJ((&USER *MSGQ *EXCL)) WAIT(5)
002200930902             MONMSG     MSGID(CPF1002) EXEC(DO)
002300931118             SNDBRKMSG  MSG('Non è possibile collegarsi con QUESTO +
002400931118                          profilo in quanto già utilizzato su altro +
002500931118                          video: questo terminale ritorna alla +
002600931118                          videata di collegamento dopo aver +
002700931118                          risposto ok + invio al presente +
002800931118                          messaggio.') TOMSGQ(&DSP) MSGTYPE(*INQ) +
002900931118                          RPYMSGQ(&DSP)
003000930902
003100930902             SIGNOFF
003200930902
003300921102             GOTO       CMDLBL(FINE)
003400930902             ENDDO
003500930903             ENDDO
003600990706 NOTEST:
003700921103 FINE:       ENDPGM
