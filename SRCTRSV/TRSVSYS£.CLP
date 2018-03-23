000100920505TRSVSYS:     PGM
000300900406             DCL        VAR(&RISP) TYPE(*CHAR) LEN(2)
000500990218             SNDUSRMSG  MSG('Montare nastro per Salvataggi "Libreria +
000600990826                          di Sistema + SAV)". Poi rispondere: OK.') +
000700990218                          VALUES(OK NO) DFT(OK) TOMSGQ(QSYSOPR) +
000800990218                          MSGRPY(&RISP)
001000900406             IF         COND(&RISP *NE 'OK') THEN(GOTO CMDLBL(ENDF))
001101990826             ENDSYS     OPTION(*IMMED)
001200991211             DLYJOB     DLY(300)
001300921123
001301020316             SAVSYS     DEV(TAP01) ENDOPT(*LEAVE) DTACPR(*YES)
001302020316
001402990826             /*           SALVATAGGIO IFS    */
001403990826
001409020316             SAV        DEV('QSYS.LIB/TAP01.DEVD') OBJ(('/*') +
001410020316                          ('/QSYS.LIB' *OMIT) ('/QDLS' *OMIT)) +
001411020316                          ENDOPT(*LEAVE)
001500900403
001502020216
001600900403 ENDF:       ENDPGM
