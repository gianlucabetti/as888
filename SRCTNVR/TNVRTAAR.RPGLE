000100040225     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200001006
000300040225      *------------------------------------------------------------------------*
000400040225      * Dati errati in bolla
000500040225      *------------------------------------------------------------------------*
000600040225
000700040302     fTitaa00f  uf   e             Disk
000800010116
000900040225     d TxtInOut        S           2048
001000040225     d ElencoChar      S            256
001100040225     d TipoElenco      S              1
001200040225     d CharSost        S              1
001300040225     d UpperCase       S              1
001400040225     d ChkNull         S              1    Inz('1')
001500040225     d CharNull        S              1
001600040225     d Esito           S              1
001700040303
001800040303     d wok             s              1    Inz(*Off)
001900040225
002000001006      *------------------------------------------------------------------------*
002100040225
002200040225do  1c                   Do        *Hival
002300040302     c                   Read      Titaa00f
002400040226
002500040226     c                   If        %eof
002600040226     c                   Leave
002700040226     c                   EndIf
002800040303
002900040303     c                   Eval      wok = *Off
003000040225
003100040302     c                   Eval      TxtInOut = TaaRsc
003200040225     c                   Call      'XCHKCHAR'
003300040225     c                   Parm                    TxtInOut
003400040225     c                   Parm                    ElencoChar
003500040225     c                   Parm                    TipoElenco
003600040225     c                   Parm                    CharSost
003700040225     c                   Parm                    UpperCase
003800040225     c                   Parm                    ChkNull
003900040225     c                   Parm                    CharNull
004000040225     c                   Parm                    Esito
004100040225     c                   If        Esito = '1'
004200040303     c                   Eval      TaaRsc = TxtInOut
004300040303     c                   Eval      wok = *On
004400040225     c                   EndIf
004500040225
004600040302     c                   Eval      TxtInOut = TaaInd
004700040225     c                   Call      'XCHKCHAR'
004800040225     c                   Parm                    TxtInOut
004900040225     c                   Parm                    ElencoChar
005000040225     c                   Parm                    TipoElenco
005100040225     c                   Parm                    CharSost
005200040225     c                   Parm                    UpperCase
005300040225     c                   Parm                    ChkNull
005400040225     c                   Parm                    CharNull
005500040225     c                   Parm                    Esito
005600040225     c                   If        Esito = '1'
005700040303     c                   Eval      TaaInd = TxtInOut
005800040303     c                   Eval      wok = *On
005900040225     c                   EndIf
006000040225
006100040302     c                   Eval      TxtInOut = TaaCap
006200040225     c                   Call      'XCHKCHAR'
006300040225     c                   Parm                    TxtInOut
006400040225     c                   Parm                    ElencoChar
006500040225     c                   Parm                    TipoElenco
006600040225     c                   Parm                    CharSost
006700040225     c                   Parm                    UpperCase
006800040225     c                   Parm                    ChkNull
006900040225     c                   Parm                    CharNull
007000040225     c                   Parm                    Esito
007100040225     c                   If        Esito = '1'
007200040303     c                   Eval      TaaCap = TxtInOut
007300040303     c                   Eval      wok = *On
007400040225     c                   EndIf
007500040225
007600040302     c                   Eval      TxtInOut = TaaLoc
007700040225     c                   Call      'XCHKCHAR'
007800040225     c                   Parm                    TxtInOut
007900040225     c                   Parm                    ElencoChar
008000040225     c                   Parm                    TipoElenco
008100040225     c                   Parm                    CharSost
008200040225     c                   Parm                    UpperCase
008300040225     c                   Parm                    ChkNull
008400040225     c                   Parm                    CharNull
008500040225     c                   Parm                    Esito
008600040225     c                   If        Esito = '1'
008700040303     c                   Eval      TaaLoc = TxtInOut
008800040303     c                   Eval      wok = *On
008900040225     c                   EndIf
009000040225
009100040303     c                   If        wok = *On
009200040302     c                   Update    Titaa000
009300040303     c                   EndIf
009400040225
009500040225     c                   EndDo
009600010202
009700040225     c                   Eval      *Inlr = *On
