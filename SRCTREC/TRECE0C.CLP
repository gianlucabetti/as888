000100991227/*----------------------------------------------------------------------------*/
000200991227/* CHIUDE I SOTTOSISTEMI IN MODO ORDINATO                                     */
000300991227/*----------------------------------------------------------------------------*/
000400991227 TRECE0C:    PGM        PARM(&KPJBA)
000500991227
000600991227/* --------- DICHIARAZIONE VARIABILI ---------------------------------------- */
000700991227             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(512)
000800991227
000900991227/* --------- CHIUDE I SOTTOSISTEMI DEL CONTO ECONOMICO ---------------------- */
001000991227             ENDSBS     SBS(GAIDTAQ)
001100991227
001200991227/* --------- LANCIA IL PROGRAMMA CHE CHIUDE IN MODO CONTROLLATO ------------- */
001300991227             CALL       PGM(TRECE0R)
001400991227
001500991227             ENDPGM
