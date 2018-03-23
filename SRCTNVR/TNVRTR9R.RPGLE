000100040318      *===============================================================*
000200040318      * TNVRTR9R * File FLTR9: conversione 5° campo chiave            *
000300040318      *===============================================================*
000400040318
000500040318     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600040318
000700040318     fFLTR900F  Up   e             disk
000800040318
000900040318if  1c                   if            TR9fl3 > *zeros
001000040318     c                             and TR9prg = *zeros
001100040318     c                   move      TR9fl3        TR9prg
001200040318     c                   clear                   TR9fl3
001300040318     c                   update    FLTR9000
001400040318e   1c                   endif
