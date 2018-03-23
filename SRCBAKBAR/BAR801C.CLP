000100960221/*-------------------------------------------------------------------*/
000200960221/*  Stampa relazioni Azioni/Menù ---> Menù Principale/Profilo        */
000300960221/*                                                                   */
000400960221/*                   - Cerca libreria file di Work x stampa          */
000500960221/*                   - Duplica file di Work in QTEMP                 */
000600960221/*                   - Richiama il programma che esegue la stampa    */
000700960221/*                   - Deleta file di Work dalla QTEMP               */
000800960221/*-------------------------------------------------------------------*/
000900960221             PGM        PARM(&KPJBA)
001000960221             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
001100960221             DCL        VAR(&LIBFIL)     TYPE(*CHAR) LEN(10)
001200960221/*                                                                   */
001300960221/*---------- Reperisco libreria di residenza oggetti -------------------------*/
001400960221             RTVMBRD    FILE(KPMNUW0F) RTNLIB(&LIBFIL)
001500960221             CRTDUPOBJ  OBJ(KPMNUW*) FROMLIB(&LIBFIL) OBJTYPE(*FILE) +
001600960221                          TOLIB(QTEMP)
001700960221/*                                                                   */
001800960221/*---------- Richiamo pgm x stampa relazioni ---------------------------------*/
001900960221             CALL       PGM(BAR801R) PARM(&KPJBA)
002000960221/*                                                                   */
002100960221/*---------- Deleto file di work dalla QTEMP ---------------------------------*/
002200960221             DLTF       FILE(QTEMP/KPMNUW*)
002300960221
002400960221             ENDPGM
