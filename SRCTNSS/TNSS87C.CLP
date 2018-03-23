000100960213/*----------------------------------------------------------------------------*/
000200960213/* STATISTICA TRAINI - LANCIO                                                 */
000300960903/*                                                                            */
000400960903/*   (1^ parte)       (2^ parte)                                              */
000500960903/* --------------'X'--------------                                            */
000600960903/* KPJPR         TAD         KPJDO                                            */
000700960213/*----------------------------------------------------------------------------*/
000800960213 TNSS87C:    PGM        PARM(&KPJBA)
000900960903
001000990608/* DICHIARAZIONI VARIABILI                                                    */
001100960927             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
001200960927             DCL        VAR(&CRE)   TYPE(*CHAR) LEN(1)
001300960903
001400990608/* VALORIZZAZIONI VARIABILI                                                   */
001500990608             CHGVAR     VAR(&CRE)   VALUE(%SUBSTRING(&KPJBA 259 1))
001600960926
001700960927/* SE RICHIESTO AGGIORNA IL FILE DI LAVORO                                    */
001800960927             IF         COND(&CRE *EQ 'S') THEN(DO)
001900960927             CALL       PGM(TNSS87R) PARM(&KPJBA)
002000960927             ENDDO
002100960213
002200960903/* ELABORA LA STATISTICA RITARDI TRAINI CON TOTALI PER AREA                   */
002300000706             CHGVAR     VAR(%SST(&KPJBA 261 1)) VALUE('A')
002400960213             CALL       PGM(TNSS88R) PARM(&KPJBA)
002500960903
002600960903/* ELABORA LA STATISTICA RITARDI TRAINI CON TOTALI PER DIVISIONE              */
002700000706             CHGVAR     VAR(%SST(&KPJBA 261 1)) VALUE('D')
002800960903             CALL       PGM(TNSS88R) PARM(&KPJBA)
002900960213
003000960213             RCLRSC     LVL(*CALLER)
003100960213             ENDPGM
