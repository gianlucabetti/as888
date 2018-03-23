000100940215/*********************************************************************/
000200940215/* LANCIO AUTOMATICO PROCEDURA PTPOSTEL                              */
000300940215/*********************************************************************/
000400950711             PGM        PARM(&KPJBA)
000401950711             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000700940603
000800940603             DCL        VAR(&SOC)   TYPE(*CHAR) LEN(3)
000900940603                        /* codice società                      */
000901950711
001000940603             DCL        VAR(&UTE)   TYPE(*CHAR) LEN(10)
001100940603                        /* nome utente                         */
001101950711
001200940603             DCL        VAR(&PSW)   TYPE(*CHAR) LEN(10)
001300940603                        /* password dell'utente                */
001301950711
001400950711             DCL        VAR(&TAGCRT) TYPE(*CHAR) LEN(1)
001500940603                        /* 1 = creazione lotti */
001501950711
001600950711             DCL        VAR(&HHCRT)  TYPE(*CHAR) LEN(6)
001700940603                        /* ora lancio ceazione lotti */
001701950711
001800950711             DCL        VAR(&DTCRT)  TYPE(*CHAR) LEN(6)
001900940603                        /* data lancio creazione lotti */
001901950711
002000950711             DCL        VAR(&TAGACC) TYPE(*CHAR) LEN(1)
002100940603                        /* 1 = raccolta posta  */
002101950711
002200950711             DCL        VAR(&HHACC)  TYPE(*CHAR) LEN(6)
002300940603                        /* ora lancio  raccolta posta */
002301950711
002400950711             DCL        VAR(&DTACC)  TYPE(*CHAR) LEN(6)
002500940603                        /* data lancio raccolta posta  */
002501950711
002600950711             DCL        VAR(&GRPACC) TYPE(*CHAR) LEN(10)
002700940603                        /* gruppo per  raccolta posta  */
002701950711
002800950711             DCL        VAR(&TAGSPE) TYPE(*CHAR) LEN(1)
002900940603                        /* 1 = spedizione lotti */
002901950711
003000950711             DCL        VAR(&HHSPE)  TYPE(*CHAR) LEN(6)
003100940603                        /* ora lancio spedizione lotti */
003101950711
003200950711             DCL        VAR(&DTSPE)  TYPE(*CHAR) LEN(6)
003300940603                        /* data lancio spedizione lotti */
003301950711
003400940603             DCL        VAR(&LIDB)  TYPE(*CHAR) LEN(10)
003500940603                        /* libreria dei dati  (es: PICCIONEDB) */
003501950711
003600940603             DCL        VAR(&LIPGM) TYPE(*CHAR) LEN(10)
003700940603                        /* libreria programmi (es: PICCIONE)   */
003701950711
003800940603             DCL        VAR(&LISPE) TYPE(*CHAR) LEN(10)
003900940603                        /* libreria spedizione (es: PTLINK)     */
003901950711
004000940608             DCL        VAR(&RGZ) TYPE(*CHAR) LEN(1)
004100940609                        /* Riorganizza file accorpamento, S=si  */
004101950711
004103021125             DCL        VAR(&JC) TYPE(*CHAR) LEN(10)
004104021125             DCL        VAR(&JCL) TYPE(*CHAR) LEN(10)
004105021125             DCL        VAR(&WJC) TYPE(*CHAR) LEN(10)
004106021125             DCL        VAR(&WJCL) TYPE(*CHAR) LEN(10)
004107021125             DCL        VAR(&JS) TYPE(*CHAR) LEN(10)
004108021125             DCL        VAR(&JSL) TYPE(*CHAR) LEN(10)
004109021125             DCL        VAR(&WJS) TYPE(*CHAR) LEN(10)
004110021125             DCL        VAR(&WJSL) TYPE(*CHAR) LEN(10)
004111021125             DCL        VAR(&OS) TYPE(*CHAR) LEN(10)
004112021125             DCL        VAR(&OSL) TYPE(*CHAR) LEN(10)
004113021125             DCL        VAR(&WOS) TYPE(*CHAR) LEN(10)
004114021125             DCL        VAR(&WOSL) TYPE(*CHAR) LEN(10)
004115021125             DCL        VAR(&OD) TYPE(*CHAR) LEN(10)
004116021125             DCL        VAR(&ODL) TYPE(*CHAR) LEN(10)
004117021125             DCL        VAR(&WOD) TYPE(*CHAR) LEN(10)
004118021125             DCL        VAR(&WODL) TYPE(*CHAR) LEN(10)
004119021125
004120950711             CHGVAR     VAR(&SOC) VALUE('BAR')
004121950711             CHGVAR     VAR(&UTE) VALUE('PICCIONE  ')
004122950711             CHGVAR     VAR(&PSW) VALUE('PICCIONE  ')
004123950711             CHGVAR     VAR(&TAGCRT) VALUE('1')
004124950711             CHGVAR     VAR(&HHCRT) VALUE('      ')
004125950711             CHGVAR     VAR(&DTCRT) VALUE('      ')
004126950711             CHGVAR     VAR(&TAGACC) VALUE(' ')
004127950711             CHGVAR     VAR(&HHACC) VALUE('      ')
004128950711             CHGVAR     VAR(&DTACC) VALUE('      ')
004129950711             CHGVAR     VAR(&GRPACC) VALUE('          ')
004130950711             CHGVAR     VAR(&TAGSPE) VALUE('1')
004131950711             CHGVAR     VAR(&HHSPE) VALUE('      ')
004132950711             CHGVAR     VAR(&DTSPE) VALUE('      ')
004133950711             CHGVAR     VAR(&LIDB) VALUE('PICCIONEDB')
004134950711             CHGVAR     VAR(&LIPGM) VALUE('PICCIONE  ')
004135950711             CHGVAR     VAR(&LISPE) VALUE('PTLINK    ')
004136950711             CHGVAR     VAR(&RGZ) VALUE('N')
004200940608
004300940603             ADDLIBLE   LIB(&LISPE)
004400940707             MONMSG     MSGID(CPF2103)
004500940603             ADDLIBLE   LIB(&LIPGM)
004600940707             MONMSG     MSGID(CPF2103)
004700940603             ADDLIBLE   LIB(&LIDB)
004800940707             MONMSG     MSGID(CPF2103)
004900940215
004901021125             CALL       PGM(PT000B) PARM(&SOC &UTE &PSW)
004902021125             CALL       PGM(PTAUTO) PARM(&TAGCRT &HHCRT &DTCRT +
004903021125                          &TAGACC &HHACC &DTACC &GRPACC &TAGSPE +
004904021125                          &HHSPE &DTSPE &RGZ)
005500940215
005600940603             RMVLIBLE   LIB(&LISPE)
005700940603             RMVLIBLE   LIB(&LIPGM)
005800940603             RMVLIBLE   LIB(&LIDB)
005900940215
006000940215             ENDPGM
