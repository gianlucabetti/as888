000100990525/*PARMS PGM(XREF00)                                                           */
000200990517             CMD        PROMPT('Interrogazione Cross Reference')
000300990519             PARM       KWD(TIPO) TYPE(*NAME) LEN(1) RSTD(*YES) +
000400990519                          VALUES(P O) MIN(0) PROMPT('Tipo +
000500990519                          interrogazione')
000600990519             PARM       KWD(OUTPUT) TYPE(*NAME) LEN(1) RSTD(*YES) +
000700990519                          VALUES(V S) MIN(0) PROMPT('Video o Stampa')
