000100101021             PGM        PARM(&ESITO)
000101101021
000102101021             DCL        VAR(&ESITO) TYPE(*DEC) LEN(15 5)
000103101021             DCL        VAR(&LIBBTLNEW) TYPE(*CHAR) LEN(10) VALUE('EDPDCPJBL')
000104101021             DCL        VAR(&LIBBTLOBJ) TYPE(*CHAR) LEN(10) VALUE('GAITRAOBJ')
000105101021             DCL        VAR(&LIBBTLSAV) TYPE(*CHAR) LEN(10) VALUE('EDPDC')
000106101021             DCL        VAR(&LIBPJDTAG) TYPE(*CHAR) LEN(10) VALUE('PJBARGRU')
000107101021             DCL        VAR(&LIBPJDTAS) TYPE(*CHAR) LEN(10) VALUE('PJBARSOC')
000108101021             DCL        VAR(&LIBPJOBJA) TYPE(*CHAR) LEN(10) VALUE('PJAI1O3')
000109101021             DCL        VAR(&LIBPJOBJC) TYPE(*CHAR) LEN(10) VALUE('PJCI1O3')
000110101021
000111101021             MONMSG     MSGID(CPF0000)
000112101021
000113101021             IF         COND(&ESITO *EQ 1) THEN(CALLSUBR SUBR(SET_TEST))
000114101021
000115101021             CHGVAR     VAR(&ESITO) VALUE(0)
000116101021
000117101021/******************************************************************************/
000118101021/*                                                                            */
000119101021/*           Se non riesco a fare queste cose è inutile continuare.           */
000120101021/*                                                                            */
000121101021/******************************************************************************/
000122101021
000123101021             ALCOBJ     OBJ((&LIBPJDTAG/ANSO100F *FILE *EXCL))
000124101021             MONMSG     MSGID(CPF1002) EXEC(CALLSUBR SUBR(ERRORE))
000125101021
000126101021             CPYF       FROMFILE(&LIBPJDTAG/ANSO100F) TOFILE(&LIBBTLNEW/ANSO100F) +
000127101021                          MBROPT(*REPLACE) FMTOPT(*MAP *DROP)
000128101021             MONMSG     MSGID(CPF0000) EXEC(CALLSUBR SUBR(ERRORE))
000147101021
000148101021             MOVOBJ     OBJ(&LIBPJDTAG/ANSO100F) OBJTYPE(*FILE) TOLIB(&LIBBTLSAV)
000149101021             MONMSG     MSGID(CPF0000) EXEC(CALLSUBR SUBR(ERRORE))
000150101021
000151101021             DLTF       FILE(&LIBPJDTAG/ANSO101L)
000152101021             MONMSG     MSGID(CPF2105)
000153101021             MONMSG     MSGID(CPF0000) EXEC(CALLSUBR SUBR(ERRORE))
000154101021
000155101021/******************************************************************************/
000156101021/*                                                                            */
000157101021/*           Oggetti da spostare in PJBARGRU.                                 */
000158101021/*                                                                            */
000159101021/******************************************************************************/
000160101021
000161101021             MOVOBJ     OBJ(&LIBBTLNEW/ANSO100F) OBJTYPE(*FILE) TOLIB(&LIBPJDTAG)
000162101021             MOVOBJ     OBJ(&LIBBTLNEW/ANSO101L) OBJTYPE(*FILE) TOLIB(&LIBPJDTAG)
000163101021
000164101021/******************************************************************************/
000165101021/*                                                                            */
000166101021/*           Oggetti da spostare in PJBARSOC.                                 */
000167101021/*                                                                            */
000168101021/******************************************************************************/
000169101021
000187101021
000188101021/******************************************************************************/
000189101021/*                                                                            */
000190101021/*           Oggetti da spostare in PJAI1O3.                                  */
000191101021/*                                                                            */
000192101021/******************************************************************************/
000193101021
000194101021             CALL       PGM(GAITRAOBJ/TNSYMVC) PARM(&LIBBTLNEW &LIBPJOBJA 'ANA*      ')
000195101021             CALL       PGM(GAITRAOBJ/TNSYMVC) PARM(&LIBBTLNEW &LIBPJOBJA 'ANG*      ')
000196101021             CALL       PGM(GAITRAOBJ/TNSYMVC) PARM(&LIBBTLNEW &LIBPJOBJA 'ANP*      ')
000197101021             CALL       PGM(GAITRAOBJ/TNSYMVC) PARM(&LIBBTLNEW &LIBPJOBJA 'CONBLK*   ')
000198101021             CALL       PGM(GAITRAOBJ/TNSYMVC) PARM(&LIBBTLNEW &LIBPJOBJA 'NDDVA*    ')
000199101021             CALL       PGM(GAITRAOBJ/TNSYMVC) PARM(&LIBBTLNEW &LIBPJOBJA 'ISW*      ')
000200101021             CALL       PGM(GAITRAOBJ/TNSYMVC) PARM(&LIBBTLNEW &LIBPJOBJA 'PJXX*     ')
000201101021
000202101021/******************************************************************************/
000203101021/*                                                                            */
000204101021/*           Oggetti da spostare in PJCI1O3.                                  */
000205101021/*                                                                            */
000206101021/******************************************************************************/
000207101021
000208101021             CALL       PGM(GAITRAOBJ/TNSYMVC) PARM(&LIBBTLNEW &LIBPJOBJC 'NDC*      ')
000217101021
000218101021/******************************************************************************/
000219101021/*                                                                            */
000220101021/*           I messaggi li devo copiare 2 volte.                              */
000221101021/*                                                                            */
000222101021/******************************************************************************/
000223101021
000224101021             MRGMSGF    FROMMSGF(&LIBBTLNEW/PROMSG) TOMSGF(&LIBBTLOBJ/PROMSG)
000225101021             MRGMSGF    FROMMSGF(&LIBBTLNEW/PROMSG) TOMSGF(&LIBPJOBJA/PROMSG)
000226101021             DLTMSGF    MSGF(&LIBBTLNEW/PROMSG)
000227101021
000228101021/******************************************************************************/
000229101021/*                                                                            */
000230101021/*           Oggetti da spostare in GAITRAOBJ.                                */
000231101021/*                                                                            */
000232101021/******************************************************************************/
000233101021
000234101021             CALL       PGM(GAITRAOBJ/TNSYMVC) PARM(&LIBBTLNEW &LIBBTLOBJ '*ALL     ')
000238101021
000239101021/******************************************************************************/
000240101021/*                                                                            */
000241101021/*           Subroutine gestione errore.                                      */
000242101021/*                                                                            */
000243101021/******************************************************************************/
000244101021
000245101021             SUBR       SUBR(ERRORE)
000246101021
000247101021                CHGVAR     VAR(&ESITO) VALUE(-1)
000248101021                DSPJOB     OUTPUT(*PRINT) OPTION(*JOBLOG)
000249101021                RETURN
000250101021
000251101021             ENDSUBR
000252101021
000253101021/******************************************************************************/
000254101021/*                                                                            */
000255101021/*           Subroutine per test.                                             */
000256101021/*                                                                            */
000257101021/******************************************************************************/
000258101021
000259101021             SUBR       SUBR(SET_TEST)
000260101021
000268101021                CHGVAR     VAR(&LIBBTLNEW) VALUE('EDPDCPJBLN')
000269101021                CHGVAR     VAR(&LIBBTLOBJ) VALUE('EDPDCPJBLO')
000270101021                CHGVAR     VAR(&LIBBTLSAV) VALUE('EDPDCPJBLV')
000271101021                CHGVAR     VAR(&LIBPJDTAG) VALUE('EDPDCPJBLG')
000272101021                CHGVAR     VAR(&LIBPJDTAS) VALUE('EDPDCPJBLS')
000273101021                CHGVAR     VAR(&LIBPJOBJA) VALUE('EDPDCPJBLA')
000274101021                CHGVAR     VAR(&LIBPJOBJC) VALUE('EDPDCPJBLC')
000275101021
000276101021             ENDSUBR
000277101021
000278101021/******************************************************************************/
000279101021/*                                                                            */
000280101021/*           Se arrivo qui vuol dire che è andato tutto bene ... forse ...    */
000281101021/*                                                                            */
000282101021/******************************************************************************/
000283101021
000284101021             ENDPGM
