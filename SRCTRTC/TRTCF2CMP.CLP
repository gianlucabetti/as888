000100981111/********************************************************************/
000200010124/* SERVE PER COMPILARE                                              */
000300981111/********************************************************************/
000400880609             PGM
000500021114             DCL        VAR(&LIBPRT) TYPE(*CHAR) LEN(10)
000600970107
000700021114             RTVOBJD    OBJ(QSYSPRT) OBJTYPE(*FILE) RTNLIB(&LIBPRT)
000800021114             CRTDUPOBJ  OBJ(QSYSPRT) FROMLIB(&LIBPRT) OBJTYPE(*FILE) +
000900021114                          TOLIB(QTEMP) NEWOBJ(FEDEXPRT)
001000021114             MONMSG     MSGID(CPF0000)
001100010124
001200021114             CRTBNDRPG  PGM(GAITRAFIL/TRTCF2R) +
001300021114                          SRCFILE(GAITRASRC/SRCTRTC) +
001400021114                          SRCMBR(TRTCF2R) GENLVL(10) TEXT(*SRCMBRTXT)
001500940329
001600010124             ENDPGM
