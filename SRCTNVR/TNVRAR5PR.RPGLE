000100040225     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200001006
000300040225      *------------------------------------------------------------------------*
000400040225      * Dati errati in bolla
000500040225      *------------------------------------------------------------------------*
000600040225
000700040303     fFiar5p0f  uf   e             Disk
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
001900040302
002000040302     d dAr5Gen       e ds
002100040225
002200001006      *------------------------------------------------------------------------*
002300040225
002400040225do  1c                   Do        *Hival
002500040303     c                   Read      Fiar5p0f
002600040226
002700040226     c                   If        %eof
002800040226     c                   Leave
002900040226     c                   EndIf
003000040303
003100040302      * solo rcd GEN
003200040302     c                   If        Ar5Trd <> 'GEN'
003300040302     c                   Iter
003400040302     c                   EndIf
003500040303
003600040303     c                   Eval      wok = *Off
003700040302     c                   Movel     Ar5Uni        dAr5Gen
003800040225
003900040302     c                   Eval      TxtInOut = 你r5Ref
004000040225     c                   Call      'XCHKCHAR'
004100040225     c                   Parm                    TxtInOut
004200040225     c                   Parm                    ElencoChar
004300040225     c                   Parm                    TipoElenco
004400040225     c                   Parm                    CharSost
004500040225     c                   Parm                    UpperCase
004600040225     c                   Parm                    ChkNull
004700040225     c                   Parm                    CharNull
004800040225     c                   Parm                    Esito
004900040225     c                   If        Esito = '1'
005000040303     c                   Eval      你r5Ref = TxtInOut
005100040303     c                   Eval      wok = *On
005200040225     c                   EndIf
005300040302
005400040302     c                   Eval      TxtInOut = 你r5Teld
005500040302     c                   Call      'XCHKCHAR'
005600040302     c                   Parm                    TxtInOut
005700040302     c                   Parm                    ElencoChar
005800040302     c                   Parm                    TipoElenco
005900040302     c                   Parm                    CharSost
006000040302     c                   Parm                    UpperCase
006100040302     c                   Parm                    ChkNull
006200040302     c                   Parm                    CharNull
006300040302     c                   Parm                    Esito
006400040302     c                   If        Esito = '1'
006500040303     c                   Eval      你r5Teld = TxtInOut
006600040303     c                   Eval      wok = *On
006700040302     c                   EndIf
006800040225
006900040303     c                   If        wok = *On
007000040302     c                   Movel     dAr5Gen       Ar5Uni
007100040303
007200040303     c                   If        Ar5Uni = *Blanks
007300040303     c                   Delete    Fiar5p00
007400040303     c                   Else
007500040303     c                   Update    Fiar5p00
007600040303     c                   EndIf
007700040303
007800040303     c                   EndIf
007900040225
008000040225     c                   EndDo
008100010202
008200040225     c                   Eval      *Inlr = *On
