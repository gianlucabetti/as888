000100000000/********************************************************************/
001400170105/* SALVA GAITRASRC SU SAVF                                     TX*/
001401170105/* X PASSAGGIO DATI SU SETRAS                                     TX*/
001402121123/********************************************************************/
002100170105 TRSVSRCC:  PGM
002300170105             DLTF       FILE(QGPL/SV_SRCXSE)
002301130530             MONMSG     MSGID(CPF0000)
002400170105             CRTSAVF    FILE(QGPL/SV_SRCXSE)
002502121123/* SALVA ...                                                  TX*/
004300170105             SAVLIB     LIB(GAITRASRC) DEV(*SAVF) +
004400170105                          SAVF(QGPL/SV_SRCXSE) TGTRLS(*CURRENT) +
004401170105                          SAVACT(*LIB) SAVACTWAIT(300) +
004402170105                          SAVACTMSGQ(EDPLB) DTACPR(*YES)
004500021227             MONMSG     MSGID(CPF0000)
009400170105             SAVRSTOBJ  OBJ(SV_SRCXSE) LIB(QGPL) RMTLOCNAME(SETRAS)
009401170105             MONMSG     MSGID(CPF0000)
009500170105             OVRPRTF    FILE(QPJOBLOG) OUTQ(QPRINT) +
009600170105                          FORMTYPE(SV_SRCXSE) USRDTA(SV_SRCXSE)
009700000000     DSPJOBLOG OUTPUT(*PRINT)
009800121123
009900000000     ENDPGM
