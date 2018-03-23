000100030227 TNSYZ8C:    PGM
000200030730
000300030730/* ____ Cancella vecchia vista logica FNORM09L in GAITRAGRU _____________________________________ */
000400030730
000500030730             DLTF       FILE(GAITRAGRPF/FNORM09L)
000600030730             MONMSG     MSGID(CPF0000)
000700030730
000800030730/* ____ Duplica nuova vista logica FNORM09L dalla EDPWK alla GAITRAGRU __________________________ */
000900030730
001000030730             CRTDUPOBJ  OBJ(FNORM09L) FROMLIB(EDPWK) OBJTYPE(*FILE) +
001100030730                          TOLIB(GAITRAGRPF)
001200030730             MONMSG     MSGID(CPF0000)
001300030730
001400030730/* ____ Elimina i file FNORM dalla EDPWK ________________________________________________________ */
001500030730
001600030730             DLTF       FILE(EDPWK/FNORM*)
001700030730             MONMSG     MSGID(CPF0000)
001800030724
001900030227             ENDPGM
