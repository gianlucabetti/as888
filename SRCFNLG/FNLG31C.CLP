000100940914             PGM        PARM(&KPJBA)
000200940927             DCL        VAR(&KPJBA)  TYPE(*CHAR) LEN(502)
000300940927 /*--------------------------------------------------------------------*/
000400940920             CHGVAR     VAR(%SUBSTRING(&KPJBA 247 1)) VALUE('A')
000500940914 /*--------------------------------------------------------------------*/
000600940914 /*  Eseguo OVRDBF su archivi giacenze in ARRIVO   per utilizzare lo   */
000700940914 /* stesso pgm. gicanze in partenza                                    */
000800050307                        OVRDBF     FILE(TIGCP01L) TOFILE(TIGCP21L)
000900940914 /*--------------------------------------------------------------------*/
001000941207 /*  Richiamo pgm INTERROGAZIONE                         */
001100941207             CALL       PGM(FNLG30R) PARM(&KPJBA)
001200941213             MONMSG     MSGID(RPG8888)
001300940914 /*--------------------------------------------------------------------*/
001400941207             DLTOVR     FILE(*ALL)
001500940914
001600940914             ENDPGM
