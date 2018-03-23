000100960213 FNLG75C:    PGM        PARM(&KPJBA)
000200941007
000300891006             DCL        VAR(&KPJBA)    TYPE(*CHAR)  LEN(502)
000400941007             DCL        VAR(&KPJBU)    TYPE(*CHAR)  LEN(256)
000500971203             DCL        VAR(&PRTFAX)   TYPE(*CHAR)  LEN(10)
000600020719             DCL        VAR(&LIVPO)    TYPE(*CHAR)  LEN(1)
000700020719             DCL        VAR(&UT§DSE) TYPE(*CHAR) LEN(1591)
000800960212             DCLF       FILE(FNLG74DS)
000801050401             DCL        VAR(&OK) TYPE(*CHAR) LEN(1)
000802050401             DCL        VAR(&DSP) TYPE(*CHAR) LEN(10)
000803050401             RTVJOBA    JOB(&DSP)
000900020719
001000020719             /* CHIAMO X§PARUT X REPERIRE IL LIVELLO DEL P.O.*/
001100020719             CALL       PGM(X§PARUT) PARM(&UT§DSE)
001200020719             CHGVAR     VAR(&LIVPO) VALUE(%SST(&UT§DSE 1512 1))
001300941007
001400960202/* Controllo il tipo di stampa richiesto                    */
001500960213             CHGVAR     VAR(&D74RIS) VALUE(%SST(&KPJBA 270 1))
001600971203             CHGVAR     VAR(&PRTFAX) VALUE(%SST(&KPJBA 275 10))
001700050321/* Esegue lo start del commit                  */
001800050321
001900050321             STRCMTCTL  LCKLVL(*CHG)
002000050321             MONMSG     MSGID(CPF8351)
002100050321
002200941007
002300960202/* Se non sono in ristampa, sono una primo livello, e devo stampare  */
002400041028/*              gli invii via FAX        eseguo OVRPRTF              */
002500020719             IF         COND((&D74RIS *NE 'R') *AND (&LIVPO *NE +
002600020719                          '2')) THEN(DO)
002700041028
002800960213                                   CHGVAR VAR(%SST(&KPJBA 271 1)) VALUE('F')
002900971203                                   OVRPRTF    FILE(LETGIC) OUTQ(&PRTFAX)
003000960213                                   CALL     PGM(FNLG75R) PARM(&KPJBA)
003001050401             MONMSG     MSGID(CPF0000) EXEC(DO)
003002050401             CHGVAR     VAR(&OK) VALUE('N')
003003050401             GOTO       CMDLBL(DOPO)
003004050401             ENDDO
003100960618                                   DLTOVR   FILE(LETGIC)
003200041028
003300960213                                   CHGVAR VAR(%SST(&KPJBA 271 1)) VALUE('N')
003400960213                                   CALL     PGM(FNLG75R) PARM(&KPJBA)
003401050401             MONMSG     MSGID(CPF0000) EXEC(DO)
003402050401             CHGVAR     VAR(&OK) VALUE('N')
003403050401             GOTO       CMDLBL(DOPO)
003404050401             ENDDO
003500041028
003600960202                                   ENDDO
003700960202
003800960213             ELSE       CMD(DO)
003900041028
004000041028/* SE SECONDO LIVELLO                                                  */
004100020719             IF         COND(&LIVPO *EQ '2') THEN(DO)
004200041028
004300960213                                   CHGVAR   VAR(%SST(&KPJBA 271 1)) VALUE('N')
004400960213                                   CALL     PGM(FNLG75R) PARM(&KPJBA)
004401050401             MONMSG     MSGID(CPF0000) EXEC(DO)
004402050401             CHGVAR     VAR(&OK) VALUE('N')
004403050401             GOTO       CMDLBL(DOPO)
004404050401             ENDDO
004500960213                                   ENDDO
004600041028
004700041028/* SE PRIMO LIVELLO                                                  */
004800041028             ELSE                  CMD(DO)
004900041028
005000960213                                   CHGVAR   VAR(%SST(&KPJBA 271 1)) VALUE('E')
005100960213                                   CALL     PGM(FNLG75R) PARM(&KPJBA)
005101050401             MONMSG     MSGID(CPF0000) EXEC(DO)
005102050401             CHGVAR     VAR(&OK) VALUE('N')
005103050401             GOTO       CMDLBL(DOPO)
005104050401             ENDDO
005200960202                                   ENDDO
005300960202
005400960213             ENDDO
005500041028
005600041028/* SE NON SONO IN RISTAMPA E DEVO INDIRIZZARE VIA E-MAIL                  */
005700041028/*               L'OVRPRTF     VERRÀ COMUNQUE   ESEGUITA SULL'RPG         */
005800041028             IF         COND(&D74RIS *NE 'R') THEN(DO)
005900041028                                   CHGVAR VAR(%SST(&KPJBA 271 1)) VALUE('M')
006000041028                                   CALL     PGM(FNLG75R) PARM(&KPJBA)
006001050401             MONMSG     MSGID(CPF0000) EXEC(DO)
006002050401             CHGVAR     VAR(&OK) VALUE('N')
006003050401             GOTO       CMDLBL(DOPO)
006100041028             ENDDO
006101050401             ENDDO
006102050401/* attiva lo STRCMTCTL ma se per qualche motivo qualche altra  */
006103050401/* procedura/programma fosse finita in maniera anomala occorre */
006104050401/* parare il colpo riducendo il male al male minore ossia      */
006105050401/* con un rollback chiudere le precedenti transazioni fatte a  */
006106050401/* metà piuttosto che confermarle. E ripartendo da una         */
006107050401/* situazione pulita.                                          */
006109050401DOPO:        IF         COND(&OK *EQ 'N') THEN(DO)
006110050401             ROLLBACK
006111050401             SNDBRKMSG  MSG('ERRORE La Stampa Elenco Giacenze Chiuse +
006112050401                          è terminata in modo anomalo. +
006113050401                          Segnalare il +
006114050401                          problema alla persona responsabile della +
006115050401                          manutenzione del programma.') TOMSGQ(&DSP)
006116050401             RCLRSC     LVL(*CALLER)
006117050401             ENDDO
006200050321/* fine controllo sincronia     */
006300050321             ENDCMTCTL
006400050321             MONMSG     MSGID(CPF8356)
006500941007
006600960202FINE:        ENDPGM
