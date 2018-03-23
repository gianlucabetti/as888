000100951124             PGM        PARM(&KNMUS &TIPO)
000200951102             DCL        VAR(&KNMUS) TYPE(*CHAR) LEN(10)
000300951124             DCL        VAR(&TIPO) TYPE(*CHAR) LEN(1)
000400951124/* &TIPO = '2' ---> RICHIAMATO DA PGM DI CONTROLLO TRAINI */
000500951124             IF         COND(&TIPO *EQ '2') THEN(DO)
000600951215             SNDUSRMSG  MSG('Controllo traini per data foglio +
000700951215                          viaggio teorica al momento non +
000800951215                          effettuabile per conflitto') MSGTYPE(*INFO)
000900951215             SNDUSRMSG  MSG('Controllo traini per data foglio +
001000951215                          viaggio teorica al momento non +
001100951215                          effettuabile per conflitto') +
001200951215                          MSGTYPE(*INFO) TOUSR(&KNMUS)
001300951124             ENDDO
001400951124/* &TIPO = '1' ---> RICHIAMATO DA PGM DI SGRIGLIAMENTO TRAINI */
001500951124             ELSE       CMD(IF COND(&TIPO *EQ '1') THEN(DO))
001600951127             SNDUSRMSG  MSG('Sgrigliamento traini al momento non +
001700951127                          effettuabile per conflitto') MSGTYPE(*INFO)
001800951127             SNDUSRMSG  MSG('Sgrigliamento traini al momento non +
001900951127                          effettuabile per conflitto') +
002000951127                          MSGTYPE(*INFO) TOUSR(&KNMUS)
002100951124             ENDDO
002200951102             ENDPGM
