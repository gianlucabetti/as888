000100990409/*------------------------------------------------------------------------------------------------*/
000200990409/* LANCIA NEL FINE SETTIMANA L'AGGIORNAMENTO DI ECFIM                                             */
000300990409/*------------------------------------------------------------------------------------------------*/
000400990409 TNVR77C1:   PGM
000500990409
000600990409/* --------- CHIUDE I SOTTOSISTEMI DEL CONTO ECONOMICO ---------------------- */
000700990409             ENDSBS     SBS(GAIDTAQ) DELAY(120)
000800990409             MONMSG     MSGID(CPF0000)
000900990409             ENDSBS     SBS(GAIDTAQSDI) DELAY(120)
001000990409             MONMSG     MSGID(CPF0000)
001100990409             DLYJOB     DLY(240)
001200990409
001300990409/* --------- STACCA I FILES DAL JOURNAL ------------------------------------- */
001400990409             ENDJRNPF   FILE(GAITRAGRU/ECFIM00F)
001500990409             MONMSG     MSGID(CPF7031) EXEC(DO)
001600990409             DLYJOB     DLY(120)
001700990409             ENDJRNPF   FILE(GAITRAGRU/ECFIM00F)
001800990409             MONMSG     MSGID(CPF0000)
001900990409             ENDDO
002000990409
002100990409/* --------- LANCIA IL PROGRAMMA DI AGGIORNAMENTO --------------------------- */
002200990409             CALL       PGM(TNVR77R)
002300990409
002400990409/* --------- ATTACCA I FILES DAL JOURNAL ------------------------------------ */
002500990409             STRJRNPF   FILE(GAITRAGRU/ECFIM00F) +
002600990409                          JRN(GAITRAGRU/JRSGRU) OMTJRNE(*OPNCLO)
002700990409             MONMSG     MSGID(CPF7030) /* No errore se già avviato */
002800990409
002900990409/* --------- APRE I SOTTOSISTEMI DEL CONTO ECONOMICO ------------------------ */
003000990409             STRSBS     SBSD(GAIDTAQ)
003100990409             STRSBS     SBSD(GAIDTAQSDI)
003200990409
003300990409             ENDPGM
