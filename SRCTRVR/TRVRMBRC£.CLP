000100900123             PGM
000200020919             CLRPFM     FILE(EDPSED/MBR)
000500020919             DSPFD      FILE(GAITRAGRU/*ALL) TYPE(*MBR) +
000600020919                          OUTPUT(*OUTFILE) FILEATR(*PF) +
000700020919                          OUTFILE(EDPSED/MBR)
000701020919             DSPFD      FILE(GAITRA201/*ALL) TYPE(*MBR) +
000702020919                          OUTPUT(*OUTFILE) FILEATR(*PF) +
000703020919                          OUTFILE(EDPSED/MBR) OUTMBR(*FIRST *ADD)
000704020919             DSPFD      FILE(SDITRA101/*ALL) TYPE(*MBR) +
000705020919                          OUTPUT(*OUTFILE) FILEATR(*PF) +
000706020919                          OUTFILE(EDPSED/MBR) OUTMBR(*FIRST *ADD)
000707020919             MONMSG     MSGID(CPF0000)
001700020919             RUNQRY     QRY(EDPSED/MBR) QRYFILE((*SAME))
001800911209
001900910115ENDPGM:      ENDPGM
