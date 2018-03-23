000100050217     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200050217      *------------------------------------------------------------------------*
000300050217      * Imposta dctdt2 x danni un UNI
000400050217      *------------------------------------------------------------------------*
000500050217     fFndct02l  uf   e           k disk
000600050217     fFnblp01l  if   e           k disk    usropn
000700050217     fFnarb01l  if   e           k disk    usropn
000800050217
000900050217     d cmd             s             60    dim(01) ctdata perrcd(1)
001000050217
001100050217     d comman          s             80
001200050217     d lenght          s             15  5 inz(80)
001300050217     d wlib            s             10    inz('FILTRA201 ')
001400050217
001500050217     c     kbol          klist
001600050217     c                   kfld                    dctaas
001700050217     c                   kfld                    dctlnp
001800050217     c                   kfld                    dctnrs
001900050217     c                   kfld                    dctnsp
002000050217
002100050217     c                   Clear                   comman
002200050217     c                   Eval      comman = cmd(1)
002300050217     c                   Eval      %Subst(comman:13:8) = 'FNBLP01L'
002400050217      * test sistema informativo di prova
002500050217     c                   Eval      %Subst(comman:30:10) = wlib
002600050217
002700050217     c                   Eval      comman =
002800050217     c                             %trim(comman) + '/FNBLP01L)'
002900050217     c                   Call      'QCMDEXC'                            99
003000050217     c                   Parm                    comman
003100050217     c                   Parm                    lenght
003200050217     c   99              Eval      *Inlr = *On
003300050217     c  n99              Open      Fnblp01l
003400050217
003500050217     c                   Clear                   comman
003600050217     c                   Eval      comman = cmd(1)
003700050217     c                   Eval      %Subst(comman:13:8) = 'FNARB01L'
003800050217      * test sistema informativo di prova
003900050217     c                   Eval      %Subst(comman:30:10) = wlib
004000050217
004100050217     c                   Eval      comman =
004200050217     c                             %trim(comman) + '/FNARB01L)'
004300050217     c                   Call      'QCMDEXC'                            99
004400050217     c                   Parm                    comman
004500050217     c                   Parm                    lenght
004600050217     c   99              Eval      *Inlr = *On
004700050217     c  n99              Open      Fnarb01l
004800050217
004900050217     c                   Do        *Hival
005000050217     c                   Read      Fndct02l
005100050217     c                   If        %Eof(Fndct02l)
005200050217     c                   Leave
005300050217     c                   EndIf
005400050217
005500050217      * pulisco sempre il campo
005600050217     c                   Clear                   dctdt2
005700050217      * se c.a. annullata o chiusa controllo se esiste ancora la bolla
005800050217     c                   If        dctatb <> *Blanks or dctdch > *Zeros
005900050217     c     kbol          Chain     Fnblp01l
006000050217     c                   If        Not %Found(Fnblp01l)
006100050217     c     kbol          Chain     Fnarb01l
006200050224     c                   If        Not %Found(Fnarb01l)
006300050217     c                   Eval      dctdt2 = *date
006400050217     c                   EndIf
006500050217     c                   EndIf
006600050217     c                   EndIf
006700050217
006800050217     c                   Except    aggiorna
006900050217
007000050217     c                   EndDo
007100050217
007200050217     c                   Eval      *Inlr = *On
007300050217
007400050217     oFndct000  e            aggiorna
007500050217     o                       dctdt2
007600050217
007700050217** cmd
007800050217OVRDBF FILE(        ) TOFILE(
