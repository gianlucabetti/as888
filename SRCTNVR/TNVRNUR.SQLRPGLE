000001021022      *PARMS CLOSQLCSR(*ENDMOD)
000100021021     H/TITLE  Trascodifica profili tabella AUT
000200020819     H DEBUG DECEDIT('0,') DATEDIT(*ymd.)
000201021002     f**********************************************************************
000206021022     f*
000300021030     f*   Cancella vecchi numeratori da 10 al 17 in FLNUF00F
000500021021     f*
000501021002     f**********************************************************************
002402021022     C/EXEC SQL
002403021030     C+ DELETE FROM GAITRA201/FLNUF00F WHERE nufcnu between 10 and 17
002406021022     C/END-EXEC
002407021030     C/EXEC SQL
002408021030     C+ DELETE FROM SDITRA101/FLNUF00F WHERE nufcnu between 10 and 17
002409021030     C/END-EXEC
008668021017     c                   seton                                        lr
