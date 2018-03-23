000100970103             PGM        PARM(&LIB$ &OBJ$ &LIB$$ &LIBAR$ &SYS$ &TIPO +
000200970110                          &SINO &FLAG &JOBQ$ &DAT &ORA &HOLD)
000300970103
000400970110             DCL        VAR(&UNO) TYPE(*CHAR) LEN(1)
000500970110             DCL        VAR(&HOLD) TYPE(*CHAR) LEN(4)
000600970110             DCL        VAR(&CONT) TYPE(*DEC) LEN(1 0) VALUE(2)
000700970110             DCL        VAR(&DAT)  TYPE(*CHAR) LEN(6)
000800970110             DCL        VAR(&ORA)  TYPE(*CHAR) LEN(6)
000900970107             DCL        VAR(&NOME) TYPE(*CHAR) LEN(8)
001000970107             DCL        VAR(&LIB$) TYPE(*CHAR) LEN(10)
001100970103             DCL        VAR(&OBJ$) TYPE(*CHAR) LEN(10)
001200970103             DCL        VAR(&LIB$$) TYPE(*CHAR) LEN(10)
001300970107             DCL        VAR(&JOBQ$) TYPE(*CHAR) LEN(10)
001400970103             DCL        VAR(&LIBAR$) TYPE(*CHAR) LEN(10)
001500970103             DCL        VAR(&SYS$) TYPE(*CHAR) LEN(10)
001600970103             DCL        VAR(&TIPO) TYPE(*CHAR) LEN(7)
001700970103             DCL        VAR(&SINO) TYPE(*CHAR) LEN(2)
001800970103             DCL        VAR(&FLAG) TYPE(*CHAR) LEN(1)
001900970103
002000900516             CHGDTAARA  DTAARA(*LDA) VALUE('                         -
002100900516                                                                     -
002200900516                                                                     -
002300900516                                                                     -
002400900516                                                             ')
002500970103             CHGDTAARA  DTAARA(*LDA (1 1)) VALUE(&FLAG)
002600970103
002700970103             CHGDTAARA  DTAARA(*LDA (11 10)) VALUE(&LIB$)
002800970103             CHGDTAARA  DTAARA(*LDA (21 10)) VALUE(&OBJ$)
002900970103             CHGDTAARA  DTAARA(*LDA (31 10)) VALUE(&LIB$$)
003000970103             CHGDTAARA  DTAARA(*LDA (41 10)) VALUE(&LIBAR$)
003100970103             CHGDTAARA  DTAARA(*LDA (51 10)) VALUE(&SYS$)
003200970103             CHGDTAARA  DTAARA(*LDA (61 7)) VALUE(&TIPO)
003300970103             CHGDTAARA  DTAARA(*LDA (71 2)) VALUE(&SINO)
003400970103
003500970107             IF         COND(&LIB$ *NE '          ') THEN(CHGVAR +
003600970107                          VAR(&NOME) VALUE(&LIB$))
003700970107
003800970107             IF         COND(&OBJ$ *NE '          ') THEN(CHGVAR +
003900970107                          VAR(&NOME) VALUE(&OBJ$))
004000970107
004100970401             CHGVAR     VAR(&CONT) VALUE(1)
004200970401INCR:        CHGVAR     VAR(&UNO) VALUE(%SST(&NOME &CONT 1))
004300970107             IF         COND(&UNO = '*') THEN(CHGVAR VAR(%SST(&NOME +
004400970107                          &CONT 1)) VALUE('.'))
004500970107             CHGVAR     VAR(&CONT) VALUE(&CONT + 1)
004600970107             IF         COND(&CONT <= 7) THEN(GOTO CMDLBL(INCR))
004700970107
004800970107
004900970110             IF         COND(&DAT <= '000000') THEN+
005000970110             (SBMJOB     CMD(CALL PGM(SNASAV)) JOB( 'S_' *CAT &NOME) +
005100970110                        JOBQ(&JOBQ$) HOLD(&HOLD))
005200970110
005300970110             IF         COND(&DAT > '000000') THEN+
005400970110             (SBMJOB     CMD(CALL PGM(SNASAV)) JOB( 'S_' *CAT &NOME) +
005500970110                        JOBQ(&JOBQ$) HOLD(&HOLD) SCDDATE(&DAT) +
005600970110                        SCDTIME(&ORA))
005700970107             ENDPGM
