000100960715             PGM        PARM(&KPJBA)
000200960715
000300960715       /*  Definisco variabili programma                   */
000400960715             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000500960910             DCL        VAR(&LIB) TYPE(*CHAR) LEN(10)
000600960715
000700960910       /*  Cerco libreria residenza TNCSBSWK               */
000800960910             RTVMBRD    FILE(TNCSBSWK)    RTNLIB(&LIB)
000900960910             CRTDUPOBJ  OBJ(TNCSBSWK)     FROMLIB(&LIB) OBJTYPE(*FILE) +
001000960910                          TOLIB(QTEMP)
001100960715
001200960910       /*  Richiamo programma scrittura file di work       */
001300960910             CALL       PGM(TNSC53R) PARM(&KPJBA)
001400960910       /*  Richiamo programma esegue stampa                */
001500960910             CALL       PGM(TNSC54R) PARM(&KPJBA)
001600960715
001700960910       /*  Cancello TNCSBSWK                               */
001800960910             DLTF       FILE(QTEMP/TNCSBSWK)
001900960910
002000960715             ENDPGM
