000100030529pgm
000200030529
000300030529             DCL        VAR(&RISP) TYPE(*CHAR) LEN(1)
000400030529
000500030529             SNDUSRMSG  MSG('<<  ATTENZIONE Le Trasmissioni conteggi +
000600030529                          Autotrasportatori non possono essere +
000700030529                          attivate poichè un utente sta utilizzando +
000800030529                          la - Conferma o la Rettifica ai Conteggi +
000900030529                          Autotrasportatori -  Verificare che tutti +
001000030529                          gli utenti siano fuori o avvisare la +
001100030529                          sede. Rispondere OK per Presa visione di +
001200030529                          questo messaggio >>') VALUES(OK) +
001300030529                          TOMSGQ(*SYSOPR) MSGRPY(&RISP)
001400030529
001500030529   endpgm
