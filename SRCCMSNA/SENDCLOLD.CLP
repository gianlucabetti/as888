000100900608  PGM        PARM(&SAVEFILE &LIB &LOCATION  &LIBOBJ &LIBAR &SINO)
000200000000             DCL        VAR(&SAVEFILE) TYPE(*CHAR) LEN(10)
000300000000             DCL        VAR(&LIB) TYPE(*CHAR) LEN(10)
000400900402             DCL            &LOCATION  *CHAR      10
000500900608             DCL            &LIBAR     *CHAR      10
000600900402             DCL            &SINO      *CHAR      1
000700900402             DCL            &LIBOBJ    *CHAR      1
000800900621             DCL            &CMD       *CHAR      160
000900900621             DCL            &CMD1      *CHAR      160
001000900402
001100900402/*   CONTROLLO ESISTENZA SAVE FIILE   **/
001200900402
001300000000CHKOBJ     OBJ(&LIB/&SAVEFILE) OBJTYPE(*FILE)
001400000000             MONMSG     MSGID(CPF0000) EXEC(DO)
001500900402             SNDUSRMSG  MSG('Save file non trovato !! ' *BCAT +
001600900402                          &SAVEFILE *CAT &LIB) MSGTYPE(*INFO) +
001700900402                          TOMSGQ(*LIBL/QSYSOPR)
001800000000                RETURN
001900000000                ENDDO
002000900402/*----------------------------------------------------------*/
002100900402/* INVIO SAVE FILE                                          */
002200900402/*----------------------------------------------------------*/
002300000000OVRSAVF    FILE(SAVEFILE) TOFILE(&LIB/&SAVEFILE)
002400900402             OVRICFDEVE PGMDEV(AA) RMTLOCNAME(&LOCATION) CMNTYPE(*APPC)
002500900608CALL       PGM(*LIBL/SEND) PARM(&SAVEFILE 'QGPL')
002600900402MONMSG  CPF0000   EXEC(DO)          /* ERRORI  */
002700911125             SNDUSRMSG  MSG('Errori in trasmissione SAVEFILE' *BCAT +
002800911125                          'locazione' *BCAT &LOCATION) +
002900911125                          MSGTYPE(*INFO) TOMSGQ(*LIBL/QSYSOPR)
003000900402             GOTO FINE
003100900402             ENDDO
003200911125             SNDUSRMSG  MSG('Trasmesso correttamente SAVEFILE:' +
003300911125                          *BCAT &SAVEFILE *BCAT &LIB *BCAT ' a +
003400911125                          locazione' *BCAT &LOCATION) +
003500911125                          MSGTYPE(*INFO) TOMSGQ(*LIBL/QSYSOPR)
003600900402IF ((&SINO = 'N') *OR (&SINO = 'n')) GOTO FINE
003700900402/*----------------------------------------------------------*/
003800900402/* RICARICAMENTO SU SISTEMA REMOTO                          */
003900900402/*----------------------------------------------------------*/
004000960130             CRTDDMF    FILE(QTEMP/TEMPSAV) RMTFILE(*LIBL/KFAZN10F) +
004100960130                          RMTLOCNAME(&LOCATION) TEXT('ddm di comodo +
004200960130                          per restore di save file')
004300900402                          MONMSG CPF7302   /* FILE GIà ESISTENTE  */
004400901003 /*          IF         COND(&LIBOBJ = 'L') THEN(DO)                */
004500901003 /*          CHGVAR     VAR(&CMD) VALUE('RSTLIB' *BCAT &LIB *BCAT + */
004600901003 /*                       'DEV(*SAVF) SAVF(QGPL/' *TCAT &SAVEFILE + */
004700901003 /*                       *TCAT ')' *BCAT 'RSTLIB(' *TCAT &LIBAR +  */
004800901003 /*                       *TCAT ')')                                */
004900901003 /*          SBMRMTCMD  CMD(&CMD) DDMFILE(TEMPSAV)                  */
005000901003 /* MONMSG  CPF0000   EXEC(DO)                                      */
005100901003 /*          SNDUSRMSG  MSG('Errori in ricaricamento SAVEFILE' +    */
005200901003 /*                       *bcat &savefile *bcat 'su sistema remoto +*/
005300901003 /*                       ') MSGTYPE(*INFO) TOMSGQ(*LIBL/QSYSOPR)   */
005400901003 /*                       GOTO FINE                                 */
005500901003 /*                    ENDDO                                        */
005600901003 /*                                           ENDDO   */
005700901003 /*          IF         COND(&LIBOBJ = 'O') THEN(DO)                */
005800900621             CHGVAR     VAR(&CMD) VALUE('RSTOBJ *ALL' *BCAT &SAVEFILE +
005900900621                          *BCAT 'DEV(*SAVF) SAVF(QGPL/' *TCAT +
006000900621                          &SAVEFILE *TCAT ')' *BCAT 'RSTLIB(' *TCAT +
006100900621                          &LIBAR *TCAT ')')
006200901003             SBMRMTCMD  CMD(&CMD) DDMFILE(TEMPSAV)
006300900402MONMSG  CPF0000   EXEC(DO)
006400911125             SNDUSRMSG  MSG('Errori in ricaricamento SAVEFILE' *BCAT +
006500911125                          &SAVEFILE *BCAT 'su sistema remoto ' +
006600911125                          *BCAT &LOCATION) MSGTYPE(*INFO) +
006700911125                          TOMSGQ(*LIBL/QSYSOPR)
006800900608                          GOTO FINE
006900900402                    ENDDO
007000901003  /*                                          ENDDO                */
007100900608             CHGVAR     VAR(&CMD1) VALUE('DLTF QGPL/' *TCAT &SAVEFILE)
007200900608             SBMRMTCMD  CMD(&CMD1) DDMFILE(TEMPSAV)
007300911125             SNDUSRMSG  MSG('Ricaricamento effettuato di' *BCAT +
007400911125                          &SAVEFILE *BCAT 'su sistema remoto ' +
007500911125                          *BCAT &LOCATION) MSGTYPE(*INFO) +
007600911125                          TOMSGQ(*LIBL/QSYSOPR)
007700900608  FINE:      RCLDDMCNV
007800900608             ENDPGM
