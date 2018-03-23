000100911031             PGM        PARM(&IND1 &IND2 &FILE       &FILNBR +
000200911031                          &DLT)
000300900907             DCL        VAR(&IND1) TYPE(*CHAR) LEN(8)
000400900907             DCL        VAR(&IND2) TYPE(*CHAR) LEN(8)
000500900907             DCL        VAR(&FILE) TYPE(*CHAR) LEN(10)
000600911031             DCL        VAR(&DLT) TYPE(*CHAR) LEN(1)
000700900907             DCL        VAR(&FILNBR) TYPE(*DEC) LEN(4)
000800900907             DCL        VAR(&JOB) TYPE(*CHAR) LEN(10)
000900900907             DCL        VAR(&JOBNBR) TYPE(*CHAR) LEN(6)
000901911031             DCL        VAR(&USER) TYPE(*CHAR) LEN(10)
001000900907
001001911031             RTVJOBA    JOB(&JOB) USER(&USER) NBR(&JOBNBR)
001100900907             SNDNETSPLF FILE(&FILE) TOUSRID((&IND1 &IND2)) +
001200900907                          JOB(&JOBNBR/&USER/&JOB) SPLNBR(&FILNBR)
001300900907             MONMSG     MSGID(CPF0000) EXEC(DO)
001301911031             SNDMSG     MSG('Spool file' *bcat &file *bcat 'non +
001302911031                          inviato a utente' *bcat &ind1 *bcat +
001303911031                          &ind2) TOUSR(*SYSOPR)
001400900907             GOTO       CMDLBL(FINE)
001500900907             ENDDO
001501911031             IF         COND(&DLT *EQ 'S') THEN(DO)
001600900907             DLTSPLF    FILE(&FILE) JOB(&JOBNBR/&USER/&JOB) +
001700900907                          SPLNBR(&FILNBR)
001800900907                          MONMSG CPF0000
001801911031                          ENDDO
001900900907FINE:
002000900907ENDPGM
