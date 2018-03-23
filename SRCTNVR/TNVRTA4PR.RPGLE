000100040225     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200001006
000300040225      *------------------------------------------------------------------------*
000400040225      * Dati errati in bolla
000500040225      *------------------------------------------------------------------------*
000600040225
000700040303     fTita4p0f  uf   e             Disk
000800010116
000900040225     d TxtInOut        S           2048
001000040225     d ElencoChar      S            256
001100040225     d TipoElenco      S              1
001200040225     d CharSost        S              1
001300040225     d UpperCase       S              1
001400040225     d ChkNull         S              1    Inz('1')
001500040225     d CharNull        S              1
001600040225     d Esito           S              1
001700040225
001800001006      *------------------------------------------------------------------------*
001900040225
002000040225do  1c                   Do        *Hival
002100040303     c                   Read      Tita4p0f
002200040226
002300040226     c                   If        %eof
002400040226     c                   Leave
002500040226     c                   EndIf
002600040303
002700040302      * no tipi rcd 'J' 'F' 'M'
002800040302     c                   If        Ta4Trc = 'J' or Ta4Trc = 'F' or
002900040302     c                             Ta4Trc = 'M'
003000040302     c                   Iter
003100040302     c                   EndIf
003200040225
003300040302     c                   Eval      TxtInOut = Ta4Not
003400040225     c                   Call      'XCHKCHAR'
003500040225     c                   Parm                    TxtInOut
003600040225     c                   Parm                    ElencoChar
003700040225     c                   Parm                    TipoElenco
003800040225     c                   Parm                    CharSost
003900040225     c                   Parm                    UpperCase
004000040225     c                   Parm                    ChkNull
004100040225     c                   Parm                    CharNull
004200040225     c                   Parm                    Esito
004300040303
004400040225     c                   If        Esito = '1'
004500040303     c                   Eval      Ta4Not = TxtInOut
004600040225
004700040302     c                   If        Ta4Not = *Blanks
004800040303     c                   Delete    Tita4p00
004900040302     c                   Else
005000040303     c                   Update    Tita4p00
005100040302     c                   EndIf
005200040303
005300040303     c                   EndIf
005400040225
005500040225     c                   EndDo
005600010202
005700040225     c                   Eval      *Inlr = *On
