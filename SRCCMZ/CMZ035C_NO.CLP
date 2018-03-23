000100900327/* ---------------------------------------------------------------*/
000200900327/*  CONTROLLA E RIPRISTINA LE LINEE                             --*/
000300900327/* ---------------------------------------------------------------*/
000400900327             PGM
000500900327             DCL        VAR(&LIN) TYPE(*CHAR) LEN(10)
000600900327             DCL        VAR(&STS)  TYPE(*DEC) LEN(5 0)
000700900507             DCL        VAR(&MSG)  TYPE(*CHAR) LEN(80)
000800900327
000900900327/* LINEA 1 */
001000900327
001100900327             RTVDTAARA  DTAARA(*LIBL/CMZ001 (59 10)) RTNVAR(&LIN)
001200900327             IF         COND(&LIN *NE ' ') THEN(DO)
001300020515             RTVCFGSTS  CFGD(&LIN) CFGTYPE(*LIN) STSCDE(&STS)
001301020515  /*     IF         COND(&STS *EQ 0 *OR &STS *EQ 10 *OR &STS *EQ + */
001400020515             IF         COND(&STS *EQ 10 *OR &STS *EQ 90 *OR &STS +
001500020515                          *EQ 80 *OR &STS *EQ 100) THEN(DO)
001600900411             VRYCFG     CFGOBJ(&LIN) CFGTYPE(*LIN) STATUS(*OFF)
001700900327             MONMSG     MSGID(CPF0000)
001800900327             VRYCFG     CFGOBJ(&LIN) CFGTYPE(*LIN) STATUS(*ON)
001900900508             MONMSG     MSGID(CPF0000) EXEC(DO)
002000900508             CHGVAR     VAR(&MSG) VALUE('Linea ' *CAT &LIN *CAT ' non +
002100900508                          attivata. Verificare o avvertire il +
002200900508                          responsabile.')
002300900508             SNDMSG     MSG(&MSG) TOMSGQ(*SYSOPR)
002400900508                          ENDDO
002500900327                          ENDDO
002600900327                          ENDDO
002700900327/* LINEA 2 */
002800900327
002900900327             RTVDTAARA  DTAARA(*LIBL/CMZ001 (69 10)) RTNVAR(&LIN)
003000900327             IF         COND(&LIN *NE ' ') THEN(DO)
003100900402             RTVCFGSTS  CFGD(&LIN) CFGTYPE(*LIN) STSCDE(&STS)
003200900402             IF         COND(&STS *EQ 0 *OR &STS *EQ 10 *OR &STS *EQ +
003300900402                          90 *OR &STS *EQ 80 *OR &STS *EQ 100) THEN(DO)
003400900327             VRYCFG     CFGOBJ(&LIN) CFGTYPE(*LIN) STATUS(*OFF)
003500900327             MONMSG     MSGID(CPF0000)
003600900327             VRYCFG     CFGOBJ(&LIN) CFGTYPE(*LIN) STATUS(*ON)
003700900508             MONMSG     MSGID(CPF0000) EXEC(DO)
003800900508             CHGVAR     VAR(&MSG) VALUE('Linea ' *CAT &LIN *CAT ' non +
003900900508                          attivata. Verificare o avvertire il +
004000900508                          responsabile.')
004100900508             SNDMSG     MSG(&MSG) TOMSGQ(*SYSOPR)
004200900508                          ENDDO
004300900327                          ENDDO
004400900327                          ENDDO
004500900327/* LINEA 3 */
004600900327
004700900327             RTVDTAARA  DTAARA(*LIBL/CMZ001 (79 10)) RTNVAR(&LIN)
004800900327             IF         COND(&LIN *NE ' ') THEN(DO)
004900900402             RTVCFGSTS  CFGD(&LIN) CFGTYPE(*LIN) STSCDE(&STS)
005000900402             IF         COND(&STS *EQ 0 *OR &STS *EQ 10 *OR &STS *EQ +
005100900402                          90 *OR &STS *EQ 80 *OR &STS *EQ 100) THEN(DO)
005200900327             VRYCFG     CFGOBJ(&LIN) CFGTYPE(*LIN) STATUS(*OFF)
005300900327             MONMSG     MSGID(CPF0000)
005400900327             VRYCFG     CFGOBJ(&LIN) CFGTYPE(*LIN) STATUS(*ON)
005500900508             MONMSG     MSGID(CPF0000) EXEC(DO)
005600900508             CHGVAR     VAR(&MSG) VALUE('Linea ' *CAT &LIN *CAT ' non +
005700900508                          attivata. Verificare o avvertire il +
005800900508                          responsabile.')
005900900508             SNDMSG     MSG(&MSG) TOMSGQ(*SYSOPR)
006000900508                          ENDDO
006100900327                          ENDDO
006200900327                          ENDDO
006300900327/* LINEA 4 */
006400900327
006500900327             RTVDTAARA  DTAARA(*LIBL/CMZ001 (89 10)) RTNVAR(&LIN)
006600900327             IF         COND(&LIN *NE ' ') THEN(DO)
006700900402             RTVCFGSTS  CFGD(&LIN) CFGTYPE(*LIN) STSCDE(&STS)
006800900402             IF         COND(&STS *EQ 0 *OR &STS *EQ 10 *OR &STS *EQ +
006900900402                          90 *OR &STS *EQ 80 *OR &STS *EQ 100) THEN(DO)
007000900327             VRYCFG     CFGOBJ(&LIN) CFGTYPE(*LIN) STATUS(*OFF)
007100900327             MONMSG     MSGID(CPF0000)
007200900327             VRYCFG     CFGOBJ(&LIN) CFGTYPE(*LIN) STATUS(*ON)
007300900508             MONMSG     MSGID(CPF0000) EXEC(DO)
007400900508             CHGVAR     VAR(&MSG) VALUE('Linea ' *CAT &LIN *CAT ' non +
007500900508                          attivata. Verificare o avvertire il +
007600900508                          responsabile.')
007700900508             SNDMSG     MSG(&MSG) TOMSGQ(*SYSOPR)
007800900508                          ENDDO
007900900327                          ENDDO
008000900327                          ENDDO
008100900427/* LINEA 5 */
008200900427
008300900427             RTVDTAARA  DTAARA(*LIBL/CMZ001 (99 10)) RTNVAR(&LIN)
008400900427             IF         COND(&LIN *NE ' ') THEN(DO)
008500900427             RTVCFGSTS  CFGD(&LIN) CFGTYPE(*LIN) STSCDE(&STS)
008600900427             IF         COND(&STS *EQ 0 *OR &STS *EQ 10 *OR &STS *EQ +
008700900427                          90 *OR &STS *EQ 80 *OR &STS *EQ 100) THEN(DO)
008800900427             VRYCFG     CFGOBJ(&LIN) CFGTYPE(*LIN) STATUS(*OFF)
008900900427             MONMSG     MSGID(CPF0000)
009000900427             VRYCFG     CFGOBJ(&LIN) CFGTYPE(*LIN) STATUS(*ON)
009100900508             MONMSG     MSGID(CPF0000) EXEC(DO)
009200900508             CHGVAR     VAR(&MSG) VALUE('Linea ' *CAT &LIN *CAT ' non +
009300900508                          attivata. Verificare o avvertire il +
009400900508                          responsabile.')
009500900508             SNDMSG     MSG(&MSG) TOMSGQ(*SYSOPR)
009600900508                          ENDDO
009700900427                          ENDDO
009800900427                          ENDDO
009900900427/* LINEA 6 */
010000900427
010100900427             RTVDTAARA  DTAARA(*LIBL/CMZ001 (109 10)) RTNVAR(&LIN)
010200900427             IF         COND(&LIN *NE ' ') THEN(DO)
010300900427             RTVCFGSTS  CFGD(&LIN) CFGTYPE(*LIN) STSCDE(&STS)
010400900427             IF         COND(&STS *EQ 0 *OR &STS *EQ 10 *OR &STS *EQ +
010500900427                          90 *OR &STS *EQ 80 *OR &STS *EQ 100) THEN(DO)
010600900427             VRYCFG     CFGOBJ(&LIN) CFGTYPE(*LIN) STATUS(*OFF)
010700900427             MONMSG     MSGID(CPF0000)
010800900427             VRYCFG     CFGOBJ(&LIN) CFGTYPE(*LIN) STATUS(*ON)
010900900508             MONMSG     MSGID(CPF0000) EXEC(DO)
011000900508             CHGVAR     VAR(&MSG) VALUE('Linea ' *CAT &LIN *CAT ' non +
011100900508                          attivata. Verificare o avvertire il +
011200900508                          responsabile.')
011300900508             SNDMSG     MSG(&MSG) TOMSGQ(*SYSOPR)
011400900508                          ENDDO
011500900427                          ENDDO
011600900427                          ENDDO
011700900427/* LINEA 7 */
011800900427
011900900427             RTVDTAARA  DTAARA(*LIBL/CMZ001 (119 10)) RTNVAR(&LIN)
012000900427             IF         COND(&LIN *NE ' ') THEN(DO)
012100900427             RTVCFGSTS  CFGD(&LIN) CFGTYPE(*LIN) STSCDE(&STS)
012200900427             IF         COND(&STS *EQ 0 *OR &STS *EQ 10 *OR &STS *EQ +
012300900427                          90 *OR &STS *EQ 80 *OR &STS *EQ 100) THEN(DO)
012400900427             VRYCFG     CFGOBJ(&LIN) CFGTYPE(*LIN) STATUS(*OFF)
012500900427             MONMSG     MSGID(CPF0000)
012600900427             VRYCFG     CFGOBJ(&LIN) CFGTYPE(*LIN) STATUS(*ON)
012700900508             MONMSG     MSGID(CPF0000) EXEC(DO)
012800900508             CHGVAR     VAR(&MSG) VALUE('Linea ' *CAT &LIN *CAT ' non +
012900900508                          attivata. Verificare o avvertire il +
013000900508                          responsabile.')
013100900508             SNDMSG     MSG(&MSG) TOMSGQ(*SYSOPR)
013200900508                          ENDDO
013300900427                          ENDDO
013400900427                          ENDDO
013500900427/* LINEA 8 */
013600900427
013700900427             RTVDTAARA  DTAARA(*LIBL/CMZ001 (129 10)) RTNVAR(&LIN)
013800900427             IF         COND(&LIN *NE ' ') THEN(DO)
013900900427             RTVCFGSTS  CFGD(&LIN) CFGTYPE(*LIN) STSCDE(&STS)
014000900427             IF         COND(&STS *EQ 0 *OR &STS *EQ 10 *OR &STS *EQ +
014100900427                          90 *OR &STS *EQ 80 *OR &STS *EQ 100) THEN(DO)
014200900427             VRYCFG     CFGOBJ(&LIN) CFGTYPE(*LIN) STATUS(*OFF)
014300900427             MONMSG     MSGID(CPF0000)
014400900427             VRYCFG     CFGOBJ(&LIN) CFGTYPE(*LIN) STATUS(*ON)
014500900508             MONMSG     MSGID(CPF0000) EXEC(DO)
014600900508             CHGVAR     VAR(&MSG) VALUE('Linea ' *CAT &LIN *CAT ' non +
014700900508                          attivata. Verificare o avvertire il +
014800900508                          responsabile.')
014900900508             SNDMSG     MSG(&MSG) TOMSGQ(*SYSOPR)
015000900508                          ENDDO
015100900427                          ENDDO
015200900427                          ENDDO
