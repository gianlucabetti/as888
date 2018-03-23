000100940914             PGM        PARM(&KPJBA)
000200940914             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000300940920             CHGVAR     VAR(%SUBSTRING(&KPJBA 247 1)) VALUE('P')
000400940914 /*--------------------------------------------------------------------*/
000500940914 /*  Richiamo pgm manuitenzione giacenze in partenza                   */
000600940914             CALL       PGM(FNLG05R) PARM(&KPJBA)
000700940914 /*--------------------------------------------------------------------*/
000800940914
000900940914             ENDPGM
