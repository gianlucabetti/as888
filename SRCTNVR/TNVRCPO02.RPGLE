000100020925     h Decedit('0,') Datedit(*dmy.) option(*nodebugio)
000200050119      *------------------------------------------------------------------------*
000300050119     fTncpo00s  if   e             Disk
000400050119     fTncps01s  if   e           k Disk
000500050119     fQsysprt   o    f  132        printer
000600050119
000700050119      *------------------------------------------------------------------------*
000800050119      *   C A M P I   D I   L A V O R O
000900050119      *------------------------------------------------------------------------*
001000050119     d wpo             s              3
001100050119
001200050119      *------------------------------------------------------------------------*
001300050119      *   S C H I E R E
001400050119      *------------------------------------------------------------------------*
001500050119     d POG             S              3    DIM(250)
001600050119
001700050119      *------------------------------------------------------------------------*
001800050119      *   D S   I N T E R N E / E S T E R N E
001900050119      *------------------------------------------------------------------------*
002000050119     d Kpjba         e ds
002100050119     d Trul31ds      e ds
002200050119
002300050119      *------------------------------------------------------------------------*
002400050119
002500050119      * leggo il file dei potenziali
002600050119
002700050119     c     *Start        Setll     Tncpo00s
002800050119do  1c                   Do        *Hival
002900050119     c                   Read      Tncpo00s
003000050119      * fine file
003100050119if  2c                   If        %Eof(Tncpo00s)
003200050119     c                   Leave
003300050119    2c                   EndIf
003400050119      * elaboro solo i potenziali dell'area 114
003500050119     c                   move      cpoflt        w003a             3
003600050119     c     w003a         Lookup    pog                                    30
003700050119if  2c                   If        Not *In30
003800050119     c                   Iter
003900050119    2c                   EndIf
004000050119      * elaboro solo i potenziali con stato blanks, 0, 1, 5, 6, 9 cau 6 e 9
004100050119     c     CpoCpo        Chain     Tncps01s
004200050119if  2c                   If        Not %Found(Tncps01s)
004300050119     c                   add       1             wnoaz
004400050119   x2c                   Else
004500050119     c                   select
004600050119if  3c                   when      Cpsfst = '0 '
004700050119     c                   add       1             waz0
004800050119if  3c                   when      CpsFst = '1 '
004900050119     c                   add       1             waz1
005000050119if  3c                   when      CpsFst = '5 '
005100050119     c                   add       1             waz5
005200050119if  3c                   when      CpsFst = '6 '
005300050119     c                   add       1             waz6
005400050119if  3c                   when      CpsFst = '9 ' and cpscnc = '06'
005500050119     c                   add       1             waz96
005600050119if  3c                   when      CpsFst = '9 ' and cpscnc = '09'
005700050119     c                   add       1             waz99
005800050119    3c                   Endsl
005900050119    2c                   EndIf
006000050119
006100050119    1c                   EndDo
006200050119
006300050119      * fine programma
006400050119     c                   except    riga
006500050119     c                   Eval      *InLr = *On
006600050119
006700050119      *------------------------------------------------------------------------*
006800050119      * ROUTINE INIZIALE
006900050119      *------------------------------------------------------------------------*
007000050119     c     *Inzsr        BegSr
007100050119
007200050119     c     *entry        plist
007300050119     c                   parm                    kpjba
007400050119
007500050119      * Memorizzo sk dei p.o. dei potenziali da estrarre
007600050119     c                   Clear                   Trul31ds
007700050119     c                   eval      I31abi = 'RA'
007800050119     c                   eval      I31cpo = 066
007900050119     c                   call      'TRUL31R'
008000050119     c                   parm                    KPJBA
008100050119     c                   parm                    TRUL31DS
008200050119     c                   movea     O31pog        pog
008300050119
008400050119     c                   clear                   wnoaz             7 0
008500050119     c                   clear                   waz0              7 0
008600050119     c                   clear                   waz1              7 0
008700050119     c                   clear                   waz5              7 0
008800050119     c                   clear                   waz6              7 0
008900050119     c                   clear                   waz96             7 0
009000050119     c                   clear                   waz99             7 0
009100050119
009200050119     c                   EndSr
009300050119     oQsysprt   e            riga             01
009400050119     o                       wnoaz         z      7
009500050119     o                       waz0          z    + 1
009600050119     o                       waz1          z    + 1
009700050119     o                       waz5          z    + 1
009800050119     o                       waz6          z    + 1
009900050119     o                       waz96         z    + 1
010000050119     o                       waz99         z    + 1
