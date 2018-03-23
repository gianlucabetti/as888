000100920915/* TAADATAC */
000200920915/* Addiziona o sottrae N giorni da una data di riferimento  */
000300920915/* &DAYS  : giorni                   */
000400920915/* &TOVAR : data di ritorno          */
000500920915/* &DATE  : data riferimento         */
000600920915/*          se 0 è oggi (TODAY)      */
000700920915/*                                                                        */
000800920623             PGM        PARM(&DAYS &TOVAR &DATE)
000900920915             DCL        VAR(&DAYS) TYPE(*DEC) LEN(5 0) /* Giorni da +
001000920915                          sottrarre o aggiungere */
001100000000             DCL        &TOVAR *CHAR LEN(6)
001200920915             DCL        VAR(&DATE) TYPE(*DEC) LEN(6 0) /* Data di +
001300920915                          riferimento */
001400000000             DCL        &WRKDAT *CHAR LEN(6)
001500000000             DCL        &JULIANA *CHAR LEN(5)
001600000000             DCL        &YRD *DEC LEN(2 0)
001700000000             DCL        &DAYSD *DEC LEN(3 0)
001800000000             DCL        &LEAP *DEC LEN(2 0)
001900000000             DCL        &DAYSINYEAR *DEC LEN(3 0)
002000000000             DCL        &NUM5 *DEC LEN(5)
002100920915             DCL        VAR(&NUM2) TYPE(*DEC) LEN(2)
002200920915                        /* &DATE=0  è oggi /*TODAY         */
002300920915             IF         COND(&DATE *EQ 0) THEN(DO) /* Trovo la data +
002400920915                          odierna/ *TODAY */
002500000000             RTVSYSVAL  SYSVAL(QDATE) RTNVAR(&WRKDAT)
002600920915             ENDDO      /* Trovo la data odierna / *TODAY */
002700000000             IF         (&DATE *NE 0) CHGVAR &WRKDAT &DATE
002800920915             CVTDAT     DATE(&WRKDAT) TOVAR(&JULIANA) TOFMT(*JUL) +
002900920915                          TOSEP(*NONE) /* Converto in Juliano */
003000920915             MONMSG     MSGID(CPF0555) EXEC(SNDPGMMSG MSGID(CPF9898) +
003100920915                          MSGF(QCPFMSG) MSGDTA('Il parametro DATA +
003200920915                          -DATE- non può essere convertito') +
003300920915                          MSGTYPE(*ESCAPE))
003400920915                        /* Sottostringa per ANNO e GIORNO */
003500000000             CHGVAR     &YRD %SST(&JULIANA 1 2)
003600000000             CHGVAR     &DAYSD %SST(&JULIANA 3 3)
003700920915             CHGVAR     VAR(&NUM5) VALUE(&DAYSD + &DAYS) /* +
003800920915                          Addiziono giorni */
003900920915 CHKPLUS:    IF         COND(&NUM5 *GT 0) THEN(GOTO CMDLBL(CHKLEAP)) +
004000920915                          /* Se positivo */
004100920915             IF         COND(&YRD *EQ 00) THEN(CHGVAR VAR(&YRD) +
004200920915                          VALUE(99)) /* Anno 2000 (Odissea nello +
004300920915                          spazio) */
004400920915             ELSE       CMD(CHGVAR VAR(&YRD) VALUE(&YRD -1)) /* +
004500920915                          Decremento l'anno */
004600920915 CHKLEAP:    CHGVAR     VAR(&NUM2) VALUE(&YRD / 4) /* Controllo +
004700920915                          l'anno se bisestile */
004800000000             CHGVAR     &LEAP (&YRD - (&NUM2 * 4))
004900000000             IF         (&LEAP *GT 0) CHGVAR &DAYSINYEAR 365
005000920915             ELSE       CMD(CHGVAR VAR(&DAYSINYEAR) VALUE(366)) /* +
005100920915                        Anno bisestile */
005200920915             IF         COND(&NUM5 *LE 0) THEN(DO) /* I giorni sono +
005300920915                        negativi */
005400000000             CHGVAR     &NUM5 (&NUM5 + &DAYSINYEAR)
005500920915             GOTO       CMDLBL(CHKPLUS) /* Controllo i giorni +
005600920915                        positivi */
005700920915             ENDDO      /* Fine giorni negativi */
005800920915             IF         COND(&NUM5 *GT &DAYSINYEAR) THEN(DO) /* +
005900920915                        Overflow */
006000920915             IF         COND(&YRD *EQ 99) THEN(CHGVAR VAR(&YRD) +
006100920915                        VALUE(-1)) /* Anno 2000 */
006200000000             CHGVAR     &YRD (&YRD + 1)  /* Bump year */
006300920915             CHGVAR     VAR(&NUM5) VALUE(&NUM5 - &DAYSINYEAR) /* +
006400920915                        Sottraggo */
006500920915             GOTO       CMDLBL(CHKLEAP) /* Testo l'anno prossimo */
006600920915             ENDDO      /* End days greater than days-in-year */
006700920915             CHGVAR     VAR(&DAYSD) VALUE(&NUM5) /* Change a 3 +
006800920915                        digits */
006900920915                        /* Substring back into Julian date */
007000000000             CHGVAR     %SST(&JULIANA 1 2) &YRD
007100000000             CHGVAR     %SST(&JULIANA 3 3) &DAYSD
007200920915             CVTDAT     DATE(&JULIANA) TOVAR(&TOVAR) FROMFMT(*JUL) +
007300920915                          TOSEP(*NONE) /* Converto nel formato del +
007400920915                          sistema */
007500000000             ENDPGM
