000100880609/********************************************************************/
000200880609/*                                                                  */
000300960830/*          - Alloca    file SSIFCSUM e SSIFTSTA -                  */
000400960830/*          -      Controlla se sono vuoti       -                  */
000500960830/*          - Se sono vuoti azzera i numeratori  -                  */
000600930507/*                                                                  */
000700880609/********************************************************************/
000800961025             PGM
000900960830
001000960830/*    Dichiaro variabile numero relativo record                */
001100960830             DCL        VAR(&NRR)    TYPE(*DEC)   LEN(10 0)
001200960830             DCL        VAR(&PULI) TYPE(*CHAR) LEN(1) VALUE(' ')
001300961025
001400961025/*     Controllo che esista SSIFCSUM                           */
001500961025             CHKOBJ     OBJ(EDFLATOUT2/SSIFCSUM) OBJTYPE(*FILE)
001600961025             MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(IFCSUMOK))
001700961025
001800961025/*  Se esiste il file controllo che non ci siano rimaste delle  */
001900961025/*                  bolle  da trascodificare                    */
002000961025             RTVMBRD    FILE(EDFLATOUT2/SSIFCSUM) NBRCURRCD(&NRR)
002100961031             MONMSG     MSGID(CPF9812) EXEC(GOTO CMDLBL(IFCSUMOK))
002200961025             IF         COND(&NRR *NE 0) THEN(GOTO CMDLBL(FINE))
002300961025
002400961025/*     Controllo che esista SSIFTSTA                           */
002500961025 IFCSUMOK:   CHKOBJ     OBJ(EDFLATOUT2/SSIFTSTA) OBJTYPE(*FILE)
002600961025             MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(IFTSTAOK))
002700961025
002800961025/*  Controllo che non ci siano date di consegna da trasmettere */
002900980129             RTVMBRD    FILE(EDFLATOUT2/SSIFTSTA) NBRCURRCD(&NRR)
003000961031             MONMSG     MSGID(CPF9812) EXEC(GOTO CMDLBL(IFTSTAOK))
003100961025             IF         COND(&NRR *NE 0) THEN(GOTO CMDLBL(FINE))
003200961025
003300960830/*    Se i file sono vuoti richiamo pgm azzeram.numeratori    */
003400961025 IFTSTAOK:   CALL       PGM(TRTC74R)
003500960830             CHGVAR     VAR(&PULI) VALUE('S')
003600960830
003700960830/*    Risottometto pgm se non ho eseguito pulizia numeratori   */
003800961025 FINE:       IF         COND(&PULI *NE 'S') THEN(SBMJOB CMD(CALL +
003900961003                          PGM(TRTC74C)) JOB(TRTC74C) +
004000961003                          JOBQ(PROBAS/KEDI) JOBPTY(7))
004100940329
004200961025             ENDPGM
