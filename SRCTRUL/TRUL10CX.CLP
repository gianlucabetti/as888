000001920804 TRUL10C:    PGM        PARM(&�SBS &ERROR &NJOB)
000002920803
000003920803             DCL        VAR(&�SBS) TYPE(*CHAR) LEN(10)
000004920803             DCL        VAR(&ERROR) TYPE(*CHAR) LEN(1)
000005920803             DCL        VAR(&NJOB) TYPE(*CHAR) LEN(6) VALUE('000000')
000006920803             DCLF       FILE(WRKSBSP)
000007920803
000008920903             ADDLIBLE   LIB(GAICMZ)
000009920903             MONMSG     MSGID(CPF0000)
000012920903
000013920803             CVTWRKSBS  OUTLIB(QTEMP)
000014920803
000015920803             OVRDBF     FILE(WRKSBSP) TOFILE(QTEMP/WRKSBSP) +
000016920803                          MBR(WRKSBSP)
000017920803READ:
000018920803             RCVF
000019920803             MONMSG     MSGID(CPF0864) EXEC(GOTO CMDLBL(ENDF))
000020920803
000021920803/* SE CI SONO LAVORI ATTIVI NEL SOTTOSISTEMA          */
000022920803
000024920803             IF         COND(&SBSBS *EQ &�SBS) THEN(DO)
000025920803
000026920803             IF         COND(&SBACTJ *GT 0) THEN(DO)
000027920803             CHGVAR     VAR(&NJOB) VALUE(&SBACTJ)
000028920803             CHGVAR     VAR(&ERROR) VALUE('2')
000029920803             GOTO       CMDLBL(ENDF)
000030920803             ENDDO
000032920803
000033920803             ENDDO
000034920803             GOTO       CMDLBL(READ)
000035920803
000036040726 ENDF:
000037040726 DLTF UGO/UGO
000100040726 ENDPGM
