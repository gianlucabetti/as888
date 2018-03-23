000100960528/********************************************************************/
000200960924/*   C O N F E R M A    B O L L E    P E R    N U M E R O     C M R */
000300960528/*                                                                  */
000400960924/*      - CONTROLLO SE DEVO RICHIAMARE IL PGM DI STAMPA TOTALI -    */
000500960924/*      - CONTROLLO SE DEVO RICHIAMARE IL PGM DI TRASFERIMENTO -    */
000600960924/*                                                                  */
000700960528/********************************************************************/
000800960528             PGM        PARM(&KPJBA)
000900960924             DCL        VAR(&KPJBA)  TYPE(*CHAR) LEN(502)
001000960924             DCL        VAR(&STAMPA) TYPE(*CHAR) LEN(1)
001100960924             DCL        VAR(&TRASFE) TYPE(*CHAR) LEN(1)
001200960926             DCL        VAR(&RIEPI)  TYPE(*CHAR) LEN(1)
001300960528
001400960924             CHGVAR     VAR(&TRASFE) VALUE(%SST(&KPJBA 294 1))
001500960924             CHGVAR     VAR(&STAMPA) VALUE(%SST(&KPJBA 295 1))
001600960926             CHGVAR     VAR(&RIEPI)  VALUE(%SST(&KPJBA 298 1))
001700960528
001800960924/*      - Controllo se devo chiamare il pgm. x stampa totali CMR.   */
001900960926             IF         COND((&STAMPA *EQ 'S') *OR (&RIEPI *EQ 'S')) +
002000960926                          THEN(CALL PGM(TRTC86R1) PARM(&KPJBA))
002100960924/*      - Controllo se devo chiamare il pgm. x trasferimento dati   */
002200960926             IF         COND(&TRASFE *NE ' ') THEN(CALL +
002300960926                          PGM(TRTC86R2) PARM(&KPJBA))
002400960731
002500960924             ENDPGM
