000100950905             PGM        PARM(&KPJBA)
000200950905             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000300030314/*           SNDBRKMSG  MSG('Inizia il Cambio Codice Commerciale: +
000400950905                          non effettuare la Manutenzione Piano dei +
000500950905                          conti e la Gestione visite/offerte fino a +
000600030314                          nuovo messaggio') TOMSGQ(*ALLWS)    */
000700950905             CALL       PGM(TRMK45R) PARM(&KPJBA)
000800030314/*           SNDBRKMSG  MSG('Terminato il Cambio Codice Commerciale: +
000900950905                          si possono riprendere tutte le +
001000030314                          elaborazioni') TOMSGQ(*ALLWS)     */
001100950905             ENDPGM
