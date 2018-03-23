000001040107/*PARMS DFTACTGRP(*YES)                                              */
000002031224/*-------------------------------------------------------------------*/
000003031224/* PGM BAK240C                                                       */
000004031224/*                                                                   */
000005031224/* GESTIONE STAMPA,CANCELLA,RIORGANIZZA KPLOG00F                     */
000006040107/* FORZO PASSAGGIO PARAMETRO A 1 PER PULIRE A PRESCINDERE DA KPDT1   */
000600000000/*-------------------------------------------------------------------*/
000801040107             PGM
000805040107
000904040107/* PULIZIA KPLOG                                                     */
000905040107             CALL       PGM(BAK851R) PARM('1')
000906040107/* RIORGANIZZAZIONE KPLOG                                            */
000907031224             RGZPFM     FILE(KPLOG00F)
000908031224             MONMSG     MSGID(CPF0000)
000909040131        /* Stampa KPLOG                                              */
000910040131             CALL       PGM(BAK852R) PARM(' ')
000911040131             MONMSG     MSGID(CPF0000)
000912040107
014900000000FINE:        ENDPGM
