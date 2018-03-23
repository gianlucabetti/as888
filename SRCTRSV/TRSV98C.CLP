000100911227TRSV98C:     PGM
000200970111             DCL        VAR(&DEV) TYPE(*CHAR) LEN(8)
000300920116             DCL        VAR(&RISP1) TYPE(*CHAR) LEN(2)
000400980728             DCL        VAR(&RISP2) TYPE(*CHAR) LEN(2)
000500920116             DCL        VAR(&RISP) TYPE(*CHAR) LEN(3)
000600911227             DCL        VAR(&NUME) TYPE(*DEC) LEN(3 0)
000700980728             DCL        VAR(&NUM) TYPE(*DEC) LEN(2 0) VALUE(1)
000800980728             DCL        VAR(&NUMF) TYPE(*DEC) LEN(2 0)
000900010508             DCL        VAR(&NOME) TYPE(*CHAR) LEN(6) VALUE('TXX900')
001000900406
001100980728             SNDUSRMSG  MSG('Montare nastri su TAPXX e rispondere +
001200980728                          03/04/09 (oppure No se non si vuole +
001300980728                          eseguire)') VALUES('03' '04' 'NO' 'no' +
001400980728                          '09') DFT('NO') TOMSGQ(*) MSGRPY(&RISP1)
001500920116
001600920116             IF         COND((&RISP1 *EQ 'NO') *OR (&RISP1 *EQ +
001700920116                          'no')) THEN(GOTO CMDLBL(NO))
001800920116
001801010508             CHGVAR     VAR(%SST(&NOME 2 2)) VALUE(&RISP1)
001900970111             IF         COND(&RISP1 *EQ '03') THEN(CHGVAR VAR(&DEV) +
002000970111                          VALUE('TAP03'))
002100970111             IF         COND(&RISP1 *EQ '04') THEN(CHGVAR VAR(&DEV) +
002200970111                          VALUE('TAP04'))
002300980728             IF         COND(&RISP1 *EQ '09') THEN(CHGVAR VAR(&DEV) +
002400980728                          VALUE('TAP09'))
002500970111             SNDUSRMSG  MSG('Fornire il numero di NASTRI che si +
002600980728                          vogliono inizializzare (max 20)') +
002700920116                          TOMSGQ(*) MSGRPY(&RISP2)
002800920116
002900970111             SNDUSRMSG  MSG('Fornire il 1° numero di NASTRO') +
003000920116                          TOMSGQ(*) MSGRPY(&RISP)
003100920116
003200911227             CHGVAR     VAR(&NUME) VALUE(&RISP)
003300920116             CHGVAR     VAR(&NUMF) VALUE(&RISP2)
003400911227             CHGVAR     VAR(%SST(&NOME 4 3)) VALUE(&NUME)
003500900403
003600970111             INZTAP     DEV(&DEV) NEWVOL(&NOME) CHECK(*NO) +
003700970111                          ENDOPT(*UNLOAD)
003800911230
003900911230SU:
004000911230             CHGVAR     VAR(&NUM) VALUE(&NUM + 1)
004100920116             IF         COND(&NUM *GT &NUMF) THEN(GOTO CMDLBL(BASTA))
004200911230
004300911230             CHGVAR     VAR(&NUME) VALUE(&NUME + 1)
004400911227             CHGVAR     VAR(%SST(&NOME 4 3)) VALUE(&NUME)
004500970111             INZTAP     DEV(&DEV) NEWVOL(&NOME) CHECK(*NO) +
004600911227                          ENDOPT(*UNLOAD)
004700911230             GOTO       CMDLBL(SU)
004800920116NO:
004900970111             SNDUSRMSG  MSG('NON SONO STATI INIZIALIZZATI i NASTRI +
005000970111                          !!!') MSGTYPE(*INFO) TOMSGQ(*)
005100910823
005200911230BASTA:       ENDPGM
