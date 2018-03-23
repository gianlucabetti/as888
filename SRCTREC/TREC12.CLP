000100021219/*----------------------------------------------------------------------------*/
000200021219/* SIMULAZIONE CONTO ECONOMICO - COMPILA IL TREC12R                           */
000300021219/*----------------------------------------------------------------------------*/
000400021219 TREC12:     PGM
000500000707
000600021219/* Esegue le OVRDBF occorrenti -----------------------------------------------*/
000700021219             OVRDBF     FILE(SISDOB) TOFILE(UNITRAGRP/SISDO01L)
000800021219             OVRDBF     FILE(SISDCB) TOFILE(UNITRAGRP/SISDC01L)
000900021219             OVRDBF     FILE(SISDOU) TOFILE(UNITRAGRP/SISDO01L)
001000021219             OVRDBF     FILE(SISDCU) TOFILE(UNITRAGRP/SISDC01L)
001100021219
001200021219/* Compila il programma ------------------------------------------------------*/
001300021219             CRTBNDRPG  PGM(GAITRAFIL/TREC12R) +
001400021219                          SRCFILE(GAITRASRC/SRCTREC) SRCMBR(TREC12R)
001500000000
001600000000             ENDPGM
