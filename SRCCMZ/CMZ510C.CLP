000100931026 CMZ510C:    PGM        PARM(&RDET5)
000200931025
000300971108             DCL        VAR(&SYSNAME) TYPE(*CHAR) LEN(8)
000400931025             DCL        VAR(&FEL)   TYPE(*DEC)  LEN(3 0)
000500931025             DCL        VAR(&ERR) TYPE(*CHAR) LEN(1)
000600931025
000700931025 /* CAMPI PER CHIAMATA CMZM05R   */
000800931025             DCL        VAR(&RTES5) TYPE(*CHAR) LEN(70)
000900931025             DCL        VAR(&NDOC5) TYPE(*DEC) LEN(7 0)
001000931025             DCL        VAR(&FCFC5) TYPE(*CHAR) LEN(1)
001100931025             DCL        VAR(&RDET5) TYPE(*CHAR) LEN(198)
001200931025             DCL        VAR(&FRIG5) TYPE(*CHAR) LEN(1)
001300931025             DCL        VAR(&NRIS5) TYPE(*DEC) LEN(7 0)
001400931025 /* CAMPI PER CHIAMATA CMZM06R   */
001500931025             DCL        VAR(&FLG6) TYPE(*CHAR) LEN(1)
001600931025             DCL        VAR(&FIL6) TYPE(*DEC) LEN(3 0)
001700931025             DCL        VAR(&LIS6) TYPE(*CHAR) LEN(10)
001800931025             DCL        VAR(&UTI6) TYPE(*CHAR) LEN(10)
001900931025             DCL        VAR(&URG6) TYPE(*CHAR) LEN(1)
002000931025             DCL        VAR(&DAT6) TYPE(*DEC) LEN(6 0)
002100931025             DCL        VAR(&ORR6) TYPE(*DEC) LEN(6 0)
002200931025             DCL        VAR(&STP6) TYPE(*CHAR) LEN(1)
002300931025             DCL        VAR(&MSG6) TYPE(*CHAR) LEN(1)
002400931025             DCL        VAR(&RIS6) TYPE(*CHAR) LEN(1)
002500931025             DCL        VAR(&URR6) TYPE(*CHAR) LEN(1)
002600931025             DCL        VAR(&MSR6) TYPE(*CHAR) LEN(1)
002700931025             DCL        VAR(&UTL6) TYPE(*CHAR) LEN(10)
002800931025
002900931025/* IMPOSTAZIONE CAMPI DA INVIARE  */
003000931025             CHGVAR     VAR(&FLG6) VALUE('I')
003100931025             CHGVAR     VAR(&FIL6) VALUE(046)
003200931025             CHGVAR     VAR(&LIS6) VALUE('          ')
003300931025             CHGVAR     VAR(&UTI6) VALUE('EDP*      ')
003400931025             CHGVAR     VAR(&URG6) VALUE('S')
003500931025             CHGVAR     VAR(&DAT6) VALUE(000000)
003600931025             CHGVAR     VAR(&ORR6) VALUE(000000)
003700931025             CHGVAR     VAR(&STP6) VALUE('N')
003800931025             CHGVAR     VAR(&MSG6) VALUE('S')
003900931025             CHGVAR     VAR(&RIS6) VALUE('N')
004000931025             CHGVAR     VAR(&URR6) VALUE('N')
004100931025             CHGVAR     VAR(&MSR6) VALUE('N')
004200931025             CHGVAR     VAR(&UTL6) VALUE('EDPCED    ')
004300931025
004400931025/*  INVIO MESSAGGIO DI FINE PROCEDURA                           */
004500931025
004600931026             ADDLIBLE   LIB(QTEMP)
004700931026             MONMSG CPF0000
004800931025             ADDLIBLE   LIB(GAICMZ)
004900931025             MONMSG CPF0000
005000931025             ADDLIBLE   LIB(GAITRAOBJ)
005100931025             MONMSG CPF0000
005200931025             ADDLIBLE   LIB(GAITRA201)
005300931025             MONMSG CPF0000
005400931025             ADDLIBLE   LIB(GAITRAAZM)
005500931025             MONMSG CPF0000
005600971115             ADDLIBLE   LIB(GAITRAGRU)
005700971115             MONMSG CPF0000
005800931025
005900931025/*  CERCO FILIALE ELABORATORE                                     */
006000971108/*           CALL       PGM(GAITRAOBJ/GIAFEL) PARM(&FEL)          */
006100971108             RTVNETA    SYSNAME(&SYSNAME)
006200971108             IF         COND(%SST(&SYSNAME 1 2) *EQ 'AS') +
006300971108                          THEN(CHGVAR VAR(&FEL) VALUE(%SST(&SYSNAME +
006400971108                          3 3)))
006500971108
006600931025
006700931025/* IMPOSTAZIONE CAMPI DA INVIARE  */
006800931025
006900931025             CHGVAR     VAR(&RTES5) VALUE('FILIALE:')
007000931025             CHGVAR     VAR(%SST(&RTES5 10 3)) VALUE(&FEL)
007100931025             CHGVAR     VAR(%SST(&RTES5 14 50)) VALUE('ERRORI USER +
007200931025                          SPACE    ')
007300931025             CHGVAR     VAR(&NDOC5) VALUE(0)
007400931025             CHGVAR     VAR(&FCFC5) VALUE(' ')
007500931025             CHGVAR     VAR(&FRIG5) VALUE('0')
007600931025             CHGVAR     VAR(&NRIS5) VALUE(0)
007700931025 /*SCRITTURA FILE */
007800931025             CALL       PGM(CMZM05R) PARM(&RTES5 &NDOC5 &FCFC5 +
007900931025                          &RDET5 &FRIG5 &NRIS5)
008000931025 /* */
008100931025 /*INVIO FILE */
008200931025             CALL       PGM(CMZM06R) PARM(&NDOC5 &FLG6 &FIL6 &LIS6 +
008300931025                          &UTI6 &URG6 &DAT6 &ORR6 &STP6 &MSG6 &RIS6 +
008400931025                          &URR6 &MSR6 &UTL6)
008500931025
008600931025FINE:        ENDPGM
