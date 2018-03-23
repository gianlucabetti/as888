000100001109 TNSYMVC:    PGM        PARM(&LIBI &LIBO &OGGETTO)
000200030326
000300950209             DCL        VAR(&LIBI) TYPE(*CHAR) LEN(10)
000400950209             DCL        VAR(&LIBO) TYPE(*CHAR) LEN(10)
000500001109             DCL        VAR(&OGGETTO) TYPE(*CHAR) LEN(10)
000600021022             DCL        VAR(&DATAFIL) TYPE(*CHAR) LEN(13)
000700021023             DCL        VAR(&DAT7FIL) TYPE(*CHAR) LEN(7)
000800021022             DCL        VAR(&DATAUNI) TYPE(*CHAR) LEN(13)
000900021023             DCL        VAR(&DAT7UNI) TYPE(*CHAR) LEN(7)
001000021022             DCL        VAR(&SYSNAM) TYPE(*CHAR) LEN(8)
001100021113             DCL        VAR(&SYS5) TYPE(*CHAR) LEN(5)
001200030326             DCL        VAR(&CMDDLTDEVS) TYPE(*CHAR) LEN(64)
001300030326             DCL        VAR(&CMDDLTDEVL) TYPE(*DEC) LEN(15 5) VALUE(64)
001400030326
001500950209             DCLF       FILE(QSYS/QADSPOBJ)
001600021022
001700021022             RTVNETA    SYSNAME(&SYSNAM)
001800021113             CHGVAR     VAR(&SYS5) VALUE(%SST(&SYSNAM 1 5))
001900910225
002000001109             IF         COND(&OGGETTO *EQ '          ') THEN(CHGVAR +
002100001109                          VAR(&OGGETTO) VALUE('*ALL'))
002200950220/*  COPIA  OGGETTI DA &LIBI A &LIBO                                  */
002300021124/*  SE IN SEDE SU AS888 E &LIBI=UNITRAGRU (EX SETRAS:NON SI VERIFICA)*/
002400021124/*                CANCELLO(CANCELLEREI)  LO STESSO OGGETTO    */
002500021023/*  DALLA GAITRAFIL SE E' CON DATA CREAZIONE INFERIORE O =         */
002600910225
002700001109             DSPOBJD    OBJ(&LIBI/&OGGETTO) OBJTYPE(*ALL) +
002800001109                          OUTPUT(*OUTFILE) OUTFILE(QTEMP/§§PROGR)
002900910226             OVRDBF     FILE(QADSPOBJ) TOFILE(QTEMP/§§PROGR)
003000910225READ:
003100910225             RCVF
003200910225             MONMSG     MSGID(CPF0864) EXEC(GOTO CMDLBL(ENDF))
003300910225
003400970327/*  ESCLUDO PGM TNSYZ8C                                            */
003500970327             IF         COND(&ODOBNM *EQ 'TNSYZ8C') THEN(GOTO +
003600970327                          CMDLBL(READ))
003700021022             IF         COND(&ODOBNM *EQ 'TNSYZ8CSE') THEN(GOTO +
003800021022                          CMDLBL(READ))
003900970327
004000030326             CHGVAR     VAR(&CMDDLTDEVS) VALUE(' ')
004100030326
004200030326/* PROGRAMMA --------------------------------------------------------*/
004300030326/*                                                                   */
004400970327             IF         COND(&ODOBTP *EQ '*PGM    ') THEN(DO)
004500030326             CHGVAR     VAR(&CMDDLTDEVS) VALUE('DLTPGM')
004600950209             DLTPGM     PGM(&LIBO/&ODOBNM)
004700910226             MONMSG     MSGID(CPF2105)
004800950209             MOVOBJ     OBJ(&LIBI/&ODOBNM) OBJTYPE(&ODOBTP) +
004900950209                          TOLIB(&LIBO)
005000910410             ENDDO
005100030326/*                                                                   */
005200030326/*-------------------------------------------------------------------*/
005300950209
005400030326/*  DSPF + PRTF -----------------------------------------------------*/
005500030326/*                                                                   */
005600021023             IF         COND(((&ODOBTP *EQ '*FILE   ') *AND (&ODOBAT +
005700021023                          *EQ 'DSPF      ')) *OR ((&ODOBTP *EQ +
005800021023                          '*FILE   ') *AND (&ODOBAT *EQ 'PRTF      +
005900021023                          '))) THEN(DO)
006000030326             CHGVAR     VAR(&CMDDLTDEVS) VALUE('DLTF')
006100950209             DLTF          (&LIBO/&ODOBNM)
006200950209             MONMSG     MSGID(CPF2105)
006300950209             MOVOBJ     OBJ(&LIBI/&ODOBNM) OBJTYPE(&ODOBTP) +
006400950209                          TOLIB(&LIBO)
006500950209             ENDDO
006600030326/*                                                                   */
006700030326/*-------------------------------------------------------------------*/
006800030326
006900030326/* MODULO -----------------------------------------------------------*/
007000030326/*                                                                   */
007100030326             IF         COND(&ODOBTP *EQ '*MODULE') THEN(DO)
007200030326             CHGVAR     VAR(&CMDDLTDEVS) VALUE('DLTMOD')
007300030326             DLTMOD     MODULE(&LIBO/&ODOBNM)
007400030326             MONMSG     MSGID(CPF2105)
007500030326             MOVOBJ     OBJ(&LIBI/&ODOBNM) OBJTYPE(&ODOBTP) +
007600030326                          TOLIB(&LIBO)
007700030326             ENDDO
007800030326/*                                                                   */
007900030326/*-------------------------------------------------------------------*/
008000030326
008100030326/* PROGRAMMA SERVIZIO -----------------------------------------------*/
008200030326/*                                                                   */
008300030326             IF         COND(&ODOBTP *EQ '*SRVPGM') THEN(DO)
008400030326             CHGVAR     VAR(&CMDDLTDEVS) VALUE('DLTSRVPGM')
008500030326             DLTSRVPGM  SRVPGM(&LIBO/&ODOBNM)
008600030326             MONMSG     MSGID(CPF2105)
008700030326             MOVOBJ     OBJ(&LIBI/&ODOBNM) OBJTYPE(&ODOBTP) +
008800030326                          TOLIB(&LIBO)
008900030326             ENDDO
009000030326/*                                                                   */
009100030326/*-------------------------------------------------------------------*/
009200030326
009300030326/* GRUPPO PANNELLI --------------------------------------------------*/
009400030326/*                                                                   */
009500030326             IF         COND(&ODOBTP *EQ '*PNLGRP') THEN(DO)
009600030326             CHGVAR     VAR(&CMDDLTDEVS) VALUE('DLTPNLGRP')
009700030326             DLTPNLGRP  PNLGRP(&LIBO/&ODOBNM)
009800030326             MONMSG     MSGID(CPF2105)
009900030326             MOVOBJ     OBJ(&LIBI/&ODOBNM) OBJTYPE(&ODOBTP) +
010000030326                          TOLIB(&LIBO)
010100030326             ENDDO
010200030326/*                                                                   */
010300030326/*-------------------------------------------------------------------*/
010400030326
010500030326/* FILE MESSAGGI ----------------------------------------------------*/
010600030326/*                                                                   */
010700030326             IF         COND(&ODOBTP *EQ '*MSGF') THEN(DO)
010800030326             CHGVAR     VAR(&CMDDLTDEVS) VALUE('DLTMSGF')
010900030326             MRGMSGF    FROMMSGF(&LIBI/&ODOBNM) TOMSGF(&LIBO/&ODOBNM)
011000030326             MONMSG     MSGID(CPF2407) EXEC(DO)
011100030326             MOVOBJ     OBJ(&LIBI/&ODOBNM) OBJTYPE(&ODOBTP) +
011200030326                          TOLIB(&LIBO)
011300030326             ENDDO
011400030326             DLTMSGF    MSGF(&LIBI/&ODOBNM)
011500030326             MONMSG     MSGID(CPF2105)
011600030326             ENDDO
011700030326/*                                                                   */
011800030326/*-------------------------------------------------------------------*/
011900030326
012000030326/* Cancello l'oggetto nella libreria di sviluppo --------------------*/
012100030326/*                                                                   */
012200030326             IF         COND(&SYS5 *EQ 'AS888' *AND &LIBI *EQ +
012300030326                          'UNITRAGRU' *AND &CMDDLTDEVS *NE ' ') +
012400030326                          THEN(DO)
012500030326             CHGVAR     VAR(&DATAFIL) VALUE('             ')
012600030326             CHGVAR     VAR(&DATAUNI) VALUE('             ')
012700030326             RTVOBJD    OBJ(GAITRAFIL/&ODOBNM) OBJTYPE(&ODOBTP) +
012800030326                          CRTDATE(&DATAFIL)
012900030326             MONMSG     MSGID(CPF0000)
013000030326             RTVOBJD    OBJ(&LIBO/&ODOBNM) OBJTYPE(&ODOBTP) +
013100030326                          CRTDATE(&DATAUNI)
013200030326             MONMSG     MSGID(CPF0000)
013300030326             CHGVAR     VAR(&DAT7FIL) VALUE(%SST(&DATAFIL 1 7))
013400030326             CHGVAR     VAR(&DAT7UNI) VALUE(%SST(&DATAUNI 1 7))
013500030326             IF         COND(&DAT7FIL *LE &DAT7UNI) THEN(DO) /* solo +
013600030326                          la data */
013700030326             CHGVAR     VAR(&CMDDLTDEVS) VALUE(&CMDDLTDEVS *BCAT +
013800030326                          'GAITRAFIL/' *CAT &ODOBNM)
013900030326             CALL       PGM(QCMDEXC) PARM(&CMDDLTDEVS &CMDDLTDEVL)
014000030326             MONMSG     MSGID(CPF0000)
014100030326             DLTDTAARA  DTAARA(GAITRAFIL/&ODOBNM)
014200030326             MONMSG     MSGID(CPF2105)
014300030326             ENDDO
014400030326             ENDDO
014500030326/*                                                                   */
014600030326/*-------------------------------------------------------------------*/
014700910405
014800910226             GOTO       CMDLBL(READ)
014900910225
015000910226ENDF:
015100910226             ENDPGM
