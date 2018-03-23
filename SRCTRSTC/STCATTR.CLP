000100000000/* Questa routine ricerca gli attributi del job   */
000200000000/* Puo' venire usata come simulazione della KPJBA */
000300000000/*                                                */
000400000000             PGM        PARM(&NMTER &USER)
000500000000             DCL        VAR(&NMTER) TYPE(*CHAR) LEN(10)
000600000000             DCL        VAR(&USER) TYPE(*CHAR) LEN(10)
000700000000             RTVJOBA    JOB(&NMTER) USER(&USER)
000800000000             ENDPGM
