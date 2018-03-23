000100041222     H DFTACTGRP(*NO) BNDDIR('TIS')
000200041221
000300041222     FTITAS30C  IP   E           K DISK
000400041222     FQSysPrt   O    F  132        PRINTER
000500041221
000600041222     D GetSpeChkCde    PR            10U 0
000700041222     D  Anno                          4
000800041222     D                                     VALUE
000900041221     D  IdSpedizione                 12
001000040226     D                                     VALUE
001100041222     D  ChkCode                      10U 0
001200041221     D  Esito                         5I 0
001300041221
001400041222     D ChkCodeMax      S             10U 0
001500041222     D ChkCode         S             10U 0
001600041221     D Esito           S              5I 0
001700041222
001800041221     C                   CLEAR                   Esito
001900041222     C                   CLEAR                   ChkCode
002000041222
002100041222     C                   CALLP     GetSpeChkCde(%EDITC(TASAAS:'X'):
002200041222     C                             %EDITC(TASLnP:'X') +
002300041221     C                             %EDITC(TASNrS:'X') + %EDITC(TASNSp:'X'):
002400041222     C                             ChkCode:Esito)
002500041222     C                   EXCEPT    Log
002600041222
002700041222     OQSysPrt   E            Log         1
002800041222     O                       TASAAS
002900041222     O                       TASLnP
003000041222     O                       TASNrS
003100041222     O                       TASNSp
003200041222     O                       ChkCode             +1
