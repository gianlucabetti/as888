000100960119 TNSY80C:    PGM        PARM(&LIB)
000200930625             DCL        VAR(&SYSNAME) TYPE(*CHAR) LEN(8)
000300911230             DCL        VAR(&LIB) TYPE(*CHAR) LEN(10)
000400001115             DCL        VAR(&DELFILE) TYPE(*CHAR) LEN(10)
000500001115             DCL        VAR(&DELMBR) TYPE(*CHAR) LEN(10)
000600001115             DCL        VAR(&SIDEL) TYPE(*CHAR) LEN(1)
000700940416             DCLF       FILE(QSYS/QAFDMBR)
000800911230
000900930625             RTVNETA    SYSNAME(&SYSNAME)
001000930625
001100971129             DSPFD      FILE(&LIB/*ALL) TYPE(*MBR) OUTPUT(*OUTFILE) +
001200001115                          FILEATR(*LF) OUTFILE(QTEMP/�FPH)
001300920504             MONMSG     MSGID(CPF3012 CPF3020) EXEC(GOTO CMDLBL(ENDF))
001400971129             DSPFD      FILE(&LIB/*ALL) TYPE(*MBR) OUTPUT(*OUTFILE) +
001500001115                          FILEATR(*PF) OUTFILE(QTEMP/�FPH) +
001600971129                          OUTMBR(*FIRST *ADD)
001700971129             MONMSG     MSGID(CPF3012 CPF3020)
001800911230
001900940416             OVRDBF     FILE(QAFDMBR) TOFILE(QTEMP/�FPH)
002000001115
002100000000READ:
002200911230             RCVF
002300911230             MONMSG     MSGID(CPF0864) EXEC(GOTO CMDLBL(ENDF))
002400001115
002500971129             IF         COND((%SST(&MBFILE 8 1)) *NE 'R' *AND +
002600021015                          (%SST(&MBFILE 8 1)) *NE 'T' *AND +
002700971129                          (%SST(&MBFILE 8 1)) *NE 'L' *AND +
002800971129                          (%SST(&MBFILE 8 1)) *NE 'C') THEN(GOTO +
002900971129                          CMDLBL(READ)) /* solo trasm. e ricez. */
003000021015
003100021015             IF         COND((%SST(&MBFILE 6 3)) *EQ 'WWT') THEN(+
003200021015                      GOTO CMDLBL(READ))
003300960119
003400021016             IF         COND(&MBFILE  *EQ 'TIVIN00R  ') THEN(+
003500021016                      GOTO CMDLBL(READ))
003600021016
003700960119             IF         COND((&MBNAME) *EQ '          ') THEN(GOTO +
003800960119                          CMDLBL(READ)) /* senza membro */
003900971129             IF         COND((%SST(&MBNAME 1 2)) *NE 'M0' *AND +
004000971129                          (%SST(&MBNAME 1 2)) *NE 'M1' *AND +
004100971129                          (%SST(&MBNAME 1 2)) *NE 'M2' *AND +
004200971129                          (%SST(&MBNAME 1 2)) *NE 'M3' *AND +
004300001115                          (%SST(&MBNAME 1 2)) *NE 'M4' *AND +
004400001115                          (%SST(&MBNAME 1 2)) *NE 'T0' *AND +
004500001115                          (%SST(&MBNAME 1 2)) *NE 'T1' *AND +
004600001115                          (%SST(&MBNAME 1 2)) *NE 'T2' *AND +
004700001115                          (%SST(&MBNAME 1 2)) *NE 'T3' *AND +
004800001115                          (%SST(&MBNAME 1 2)) *NE 'T4') THEN(GOTO +
004900001115                          CMDLBL(READ)) /* solo membri M0.. > T4.. */
005000960119
005100001115
005200001115/* VISTE LOGICHE*/
005300001115CONTR:       IF         COND(&MBFTYP = 'L') +
005400001115                          THEN(DO) /* file logico */
005500001115
005600001115/* STO LEGGENDO LO STESSO FILE LOGICO CON STESSO MEMBRO (CHE PUNTA A +
005700001115   PIU' FILE FISICI  */
005800021008             IF         COND((&MBFILE = &DELFILE) *AND (&MBNAME = +
005900021008                          &DELMBR)) THEN(DO)
006000001115
006100001115/* VOCE INDICE = 0 SI PUO' CANCELLARE PER IL MOMENTO           */
006200001115             IF         COND((&MBBOR *EQ 0) *AND (&SIDEL *NE 'N')) +
006300001115                          THEN(CHGVAR VAR(&SIDEL) VALUE('S'))
006400001115             ELSE       CMD(CHGVAR VAR(&SIDEL) VALUE('N'))
006500001115             ENDDO
006600001115
006700001115/* CAMBIATO FILE O MEMBRO FILE   */
006800001115             ELSE       CMD(DO)
006900001115
007000001115   /* SE LO POSSO CANCELLARE RIMUOVO IL MEMBRO    */
007100001115             IF         COND(&SIDEL='S') THEN(DO)
007200001115             RMVM       FILE(&LIB/&DELFILE) MBR(&DELMBR)
007300001115             MONMSG     MSGID(CPF0000)
007400001115             ENDDO
007500001115
007600001115/* IMPOSTO DATI NUOVO FILE    */
007700001115             CHGVAR     VAR(&SIDEL) VALUE(' ')
007800001115             CHGVAR     VAR(&DELFILE) VALUE(&MBFILE)
007900001115             CHGVAR     VAR(&DELMBR) VALUE(&MBNAME)
008000001115             GOTO       CMDLBL(CONTR)
008100001115             ENDDO
008200001115             ENDDO
008300021009
008400021009   /* SE DEVO, RIMUOVO L'ULTIMO MEMBRO DEI LOGICI */
008500021009             IF         COND(&SIDEL='S') THEN(DO)
008600021009             RMVM       FILE(&LIB/&DELFILE) MBR(&DELMBR)
008700021009             MONMSG     MSGID(CPF0000)
008800021009             CHGVAR     VAR(&SIDEL) VALUE(' ')
008900021009             ENDDO
009000001115
009100001115/* FILES FISICI */
009200000911             IF         COND((&MBNRCD *EQ 0) *AND (&MBFTYP = 'P')) +
009300000911                          THEN(DO) /* file fisico con record a 0 */
009400960119             RMVM       FILE(&LIB/&MBFILE) MBR(&MBNAME)
009500940416             MONMSG     MSGID(CPF0000)
009600960119             ENDDO
009700940416
009800911230             GOTO       CMDLBL(READ)
009900911230
010000930701 ENDF:
010100930701             ENDPGM
