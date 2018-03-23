000100900608  PGM        PARM(&SAVEFILE &LIB &LOCATION  &LIBOBJ &LIBAR &SINO)
000200000000             DCL        VAR(&SAVEFILE) TYPE(*CHAR) LEN(10)
000300000000             DCL        VAR(&LIB) TYPE(*CHAR) LEN(10)
000400900402             DCL            &LOCATION  *CHAR      10
000500900608             DCL            &LIBAR     *CHAR      10
000600900402             DCL            &SINO      *CHAR      1
000700900402             DCL            &LIBOBJ    *CHAR      1
000800900621             DCL            &CMD       *CHAR      160
000900900621             DCL            &CMD1      *CHAR      160
001000970108             DCL &USER  *CHAR 10
001100970108
001200970108             RTVJOBA    USER(&USER)
001300900402
001400900402/*   CONTROLLO ESISTENZA SAVE FIILE   **/
001500900402
001600000000CHKOBJ     OBJ(&LIB/&SAVEFILE) OBJTYPE(*FILE)
001700000000             MONMSG     MSGID(CPF0000) EXEC(DO)
001800970108             SNDUSRMSG  MSG(&USER *BCAT +
001900970108                           '-Save file non trovato !! ' *BCAT +
002000900402                          &SAVEFILE *CAT &LIB) MSGTYPE(*INFO) +
002100900402                          TOMSGQ(*LIBL/QSYSOPR)
002200000000                RETURN
002300000000                ENDDO
002400900402/*----------------------------------------------------------*/
002500900402/* INVIO SAVE FILE                                          */
002600900402/*----------------------------------------------------------*/
002700000000OVRSAVF    FILE(SAVEFILE) TOFILE(&LIB/&SAVEFILE)
002800900402             OVRICFDEVE PGMDEV(AA) RMTLOCNAME(&LOCATION) CMNTYPE(*APPC)
002900900608CALL       PGM(*LIBL/SEND) PARM(&SAVEFILE 'QGPL')
003000900402MONMSG  CPF0000   EXEC(DO)          /* ERRORI  */
003100970108             SNDUSRMSG  MSG(&USER *BCAT +
003200971210                           '-ERRORI IN TRASMISSIONE savefile' *BCAT +
003300911125                          'locazione' *BCAT &LOCATION) +
003400911125                          MSGTYPE(*INFO) TOMSGQ(*LIBL/QSYSOPR)
003500900402             GOTO FINE
003600900402             ENDDO
003700970108             SNDUSRMSG  MSG(&USER *BCAT +
003800970108                         '-Trasmesso correttamente SAVEFILE:' +
003900970108                          *BCAT &SAVEFILE *BCAT 'a +
004000970108                       locazione' *BCAT &LOCATION *BCAT 'a libreria' +
004100970108                    *BCAT &LIBAR)  MSGTYPE(*INFO) TOMSGQ(*LIBL/QSYSOPR)
004200900402IF ((&SINO = 'N') *OR (&SINO = 'n')) GOTO FINE
004300900402/*----------------------------------------------------------*/
004400900402/* RICARICAMENTO SU SISTEMA REMOTO                          */
004500900402/*----------------------------------------------------------*/
004600960130             CRTDDMF    FILE(QTEMP/TEMPSAV) RMTFILE(*LIBL/KFAZN10F) +
004700960130                          RMTLOCNAME(&LOCATION) TEXT('ddm di comodo +
004800960130                          per restore di save file')
004900900402                          MONMSG CPF7302   /* FILE GIà ESISTENTE  */
005000901003 /*          IF         COND(&LIBOBJ = 'L') THEN(DO)                */
005100901003 /*          CHGVAR     VAR(&CMD) VALUE('RSTLIB' *BCAT &LIB *BCAT + */
005200901003 /*                       'DEV(*SAVF) SAVF(QGPL/' *TCAT &SAVEFILE + */
005300901003 /*                       *TCAT ')' *BCAT 'RSTLIB(' *TCAT &LIBAR +  */
005400901003 /*                       *TCAT ')')                                */
005500901003 /*          SBMRMTCMD  CMD(&CMD) DDMFILE(TEMPSAV)                  */
005600901003 /* MONMSG  CPF0000   EXEC(DO)                                      */
005700901003 /*          SNDUSRMSG  MSG('Errori in ricaricamento SAVEFILE' +    */
005800901003 /*                       *bcat &savefile *bcat 'su sistema remoto +*/
005900901003 /*                       ') MSGTYPE(*INFO) TOMSGQ(*LIBL/QSYSOPR)   */
006000901003 /*                       GOTO FINE                                 */
006100901003 /*                    ENDDO                                        */
006200901003 /*                                           ENDDO   */
006300901003 /*          IF         COND(&LIBOBJ = 'O') THEN(DO)                */
006400900621             CHGVAR     VAR(&CMD) VALUE('RSTOBJ *ALL' *BCAT &SAVEFILE +
006500900621                          *BCAT 'DEV(*SAVF) SAVF(QGPL/' *TCAT +
006600900621                          &SAVEFILE *TCAT ')' *BCAT 'RSTLIB(' *TCAT +
006700900621                          &LIBAR *TCAT ')')
006800901003             SBMRMTCMD  CMD(&CMD) DDMFILE(TEMPSAV)
006900900402MONMSG  CPF0000   EXEC(DO)
007000970108             SNDUSRMSG  MSG(&USER *BCAT +
007100971210                          '-ERRORI IN RICARICAMENTO savefile' *BCAT +
007200911125                          &SAVEFILE *BCAT 'su sistema remoto ' +
007300911125                          *BCAT &LOCATION) MSGTYPE(*INFO) +
007400911125                          TOMSGQ(*LIBL/QSYSOPR)
007500900608                          GOTO FINE
007600900402                    ENDDO
007700901003  /*                                          ENDDO                */
007800900608             CHGVAR     VAR(&CMD1) VALUE('DLTF QGPL/' *TCAT &SAVEFILE)
007900900608             SBMRMTCMD  CMD(&CMD1) DDMFILE(TEMPSAV)
008000970108             SNDUSRMSG  MSG(&USER *BCAT '-Ricaricamento +
008100970108                          effettuato di' *BCAT &SAVEFILE *BCAT +
008200970108                          'su sistema remoto ' *BCAT &LOCATION +
008300970108                          *BCAT 'in libreria ' *bcat &LIBAR) +
008400970108                          MSGTYPE(*INFO) TOMSGQ(*LIBL/QSYSOPR)
008500900608  FINE:      RCLDDMCNV
008600900608             ENDPGM
