000100040220     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200040220
000300040220     fFiarbf1c  uf   e           k Disk
000400040223     f                                     Ignore(Fnarbd00)
000500040223     f                                     Ignore(Fnarbg00)
000600040223     f                                     Ignore(Fnarbk00)
000700040223     f                                     Ignore(Fnarbp00)
000800040223     f                                     Ignore(Fiarbt00)
000900040220     f                                     Ignore(Fnarbv00)
001000040223
001100040223     d dsarbm        e ds                  Extname(fnarbm0f)
001200040223     d                                     Inz
001300040220
001400040223     d dsarbmo       e ds                  Extname(fnarbm0f)
001500040223     d                                     Prefix(o_)
001600040223     d                                     Inz
001700040223
001800040223     d sav_ArbFst      s                   Like(ArbFst)
001900040223
002000040220      *---------------------------------------------------------------*
002100040206
002200040223      * --> FNARBM
002300040220do  1c                   Do        *Hival
002400040220     c                   Read      Fiarbf1c
002500040223      * fine file
002600040220     c                   If        %Eof
002700040220     c                   Leave
002800040223     c                   EndIf
002900040223      * controllo rcd
003000040223     c                   If        dsarbm <> dsarbmo
003100040223     c                   Eval      sav_ArbFst = ArbFst
003200040223      * se diverso solo x flag stampa devo cancellare
003300040223     c                   Eval      Arbfst = o_Arbfst
003400040223     c                   If        dsarbm =  dsarbmo
003500040223     c                   Delete    Fnarbm00
003600040223      * se ho cancellato il rcd con FST = 'S' devo aggiornare il rcd precedente
003700040223     c                   If        sav_ArbFst = 'S'
003800040223     c                   Readp     Fiarbf1c
003900040223     c                   Eval      ArbFst = sav_ArbFst
004000040223     c                   Update    Fnarbm00
004100040223     c                   Iter
004200040223     c                   EndIf
004300040223     c                   EndIf
004400040223     c                   Eval      ArbFst = sav_ArbFst
004500040223     c                   Eval      dsarbmo = dsarbm
004600040223      * elimino rcd doppio
004700040220     c                   Else
004800040223     c                   Delete    Fnarbm00
004900040220     c                   EndIf
005000040223
005100040223    1c                   EndDo
005200040220
005300040220     c                   Eval      *InLr = *On
