000100970502 TNSY52C:    PGM        PARM(&KPJBA)
000200940207             DCL        VAR(&KPJBA)    TYPE(*CHAR)  LEN(502)
000300950807             DCL        VAR(&LIBRIF)    TYPE(*CHAR)  LEN(10)
000400950807             DCL        VAR(&LIBCTR)    TYPE(*CHAR)  LEN(10)
000600950807 /* NELLA KPJBU BISOGNA INDICARE     :                                        */
000700950808 /* LIBRERIA DI RIFERIMENTO:  257-266                                         */
000800950808 /* LIBRERIA DA CONTROLLARE:  277-286                                         */
001000900412
001100950808             CHGVAR     VAR(&LIBRIF) VALUE(%SST(&KPJBA 257 10))
001200950808             CHGVAR     VAR(&LIBCTR) VALUE(%SST(&KPJBA 277 10))
001400950807
001500970502/* 1) CREA FILE TNSY52F/QTEMP CON LE AZIONI   DELLA LIBRERIA DI RIFERIMENTO   */
001600970502/*                                                                            */
001700970502/* 2) ACCODA    TNSY52F/QTEMP CON LE AZIONI   DELLA LIBRERIA DA CONTROLLARE   */
001800970502/*                                                                            */
001900970502/* 3) STAMPA TUTTI LE AZIONI CHE ESISTONO IN ENTRAMBE                         */
002100970502/*                                                                            */
002200950807
002201970502             CPYF       FROMFILE(&LIBRIF/KFAZN10F) +
002202970502                          TOFILE(QTEMP/TNSY52F) CRTFILE(*YES) +
002203970502                          INCREL((*IF ATKA1 *NE 'A'))
002600900412
002601970502             CPYF       FROMFILE(&LIBCTR/KFAZN10F) +
002602970502                          TOFILE(QTEMP/TNSY52F) MBROPT(*ADD) +
002603970502                          CRTFILE(*NO) INCREL((*IF ATKA1 *NE 'A'))
002605970502
003000950807
003100950807/*   CREA V.L. X FILE TNSYFIL                                                 */
003200950807
003300970502             CRTLF      FILE(QTEMP/TNSY52L) +
003400950808                          SRCFILE(GAITRASRC/SRCTNSY) +
003500970502                          SRCMBR(TNSY52L) OPTION(*NOLIST *NOSRC)
003600950807
003700950807/*   RICHIAMA PGM DI CONTROLLO                                                */
003800950807
003900970502             CALL       PGM(TNSY52R) PARM(&KPJBA)
004000950807
004100901214ENDPGM:      ENDPGM
