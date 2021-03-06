000100931011             PGM        PARM(&LBD &FLD &MBD &TRA &FTR &CLR)
000101931011
000200900327             DCL        VAR(&LBD) TYPE(*CHAR) LEN(10)
000300900327             DCL        VAR(&FLD) TYPE(*CHAR) LEN(10)
000400900327             DCL        VAR(&MBD) TYPE(*CHAR) LEN(10)
000500900327             DCL        VAR(&TRA) TYPE(*CHAR) LEN(10)
000600900327             DCL        VAR(&FTR) TYPE(*CHAR) LEN(1)
000700900327             DCL        VAR(&CLR) TYPE(*CHAR) LEN(1)
000701931011
000800931011/* ESEGUO STORICIZZAZIONE SOLO SE CLR = S  OPPURE  N              */
000900931011/* SE FTR = S ESEGUO MOVOBJ DA GAICMZ A GAICMZHST                 */
001000931011/* ALTRIMENTI ESEGUO CPYF DA FILE DI PARTENZA A GAICMZHST         */
001100931007/*  SE &CLR=S  O 1 ESEGUO IL CLEAR DEL FILE DI PARTENZA           */
001200900327
001300931007             IF         COND(&FTR = 'S') THEN(DO)
001400931007             IF         COND(&CLR = 'S' *OR &CLR = 'N') THEN(DO)
001500900606             ALCOBJ     OBJ((GAICMZ/&TRA *FILE *EXCL)) WAIT(32767)
001600900606             MONMSG CPF0000
001700900327                  MOVOBJ     OBJ(GAICMZ/&TRA) OBJTYPE(*FILE) +
001800900327                              TOLIB(GAICMZHST)
001900920928             MONMSG     MSGID(CPF0000) EXEC(DO)
002000921123             DLTF GAICMZ/&TRA
002100920928             MONMSG CPF0000
002200920928             ENDDO
002300931007                  ENDDO
002400921123             DLTUSRSPC  USRSPC(GAICMZ/&TRA)
002500921123             MONMSG CPF0000
002600931007                  ENDDO
002700900327             ELSE       CMD(DO)
002800931007             IF         COND(&CLR = 'S' *OR &CLR = 'N') THEN(DO)
002900900606             ALCOBJ     OBJ((&LBD/&FLD *FILE *EXCL &MBD)) WAIT(32767)
003000900606             MONMSG CPF0000
003100900327                CPYF       FROMFILE(&LBD/&FLD) TOFILE(GAICMZHST/&TRA) +
003200900327                          FROMMBR(&MBD) TOMBR(*FROMMBR) +
003300900327                          MBROPT(*REPLACE) CRTFILE(*YES)
003400920527                          MONMSG CPF2817
003500931007                      ENDDO
003600931007             IF         COND(&CLR = 'S' *OR &CLR = '1') THEN(DO)
003700900327                      CLRPFM     FILE(&LBD/&FLD) MBR(&MBD)
003800900327                      ENDDO
003801931011
003802931011                ENDDO
003803931011
003804931011/* CANCELLO SE NON STORICIZZO */
003805931011             DLTF GAICMZ/&TRA
003806931011             MONMSG CPF0000
003807931011             DLTUSRSPC  USRSPC(GAICMZ/&TRA)
003808931011             MONMSG CPF0000
003809931011
004000900327
004100900327ENDPGM
