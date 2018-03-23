000100001120     H DECEDIT('0,') DATEDIT(*YMD.)
000200001120
000300001120      *****************************************************************
000400001120      *                                                               *
000500030210      *       Trascodifica spese incasso da LIT. a EURO               *
000600001120      *                                                               *
000700001120      *****************************************************************
000800001120
000900030210     fTfind00f  uf   e           k Disk
001000030210
001100030210     d Kut             s                   Like(IndKut) Inz(1)
001200030210     d Kcc             s                   Like(IndKcc) Inz(151)
001300030210
001400030211     d w0052           s              5  2
001500030210     d w0053           s              5  3
001600030210
001700030210      *------------------------------------------------------------------------*
001800030210
001900030210     c     kInd          Klist
002000030210     c                   Kfld                    Kut
002100030210     c                   Kfld                    Kcc
002200021216
002300030210     c     kInd          Setll     Tfind00f
002400021216     c                   Do        *Hival
002500030210     c     kInd          Reade     Tfind00f
002600030210      * Fine file
002700030210     c                   If        %Eof(Tfind00f)
002800021216     c                   Leave
002900021216     c                   EndIf
003000030210      * Rileggo se annullato
003100030210     c                   If        IndFlg <> *Blanks
003200030210     c                   Iter
003300030210     c                   EndIf
003400030210      * Rileggo se non ha spese incasso
003500030210     c                   If        IndSin = *Zeros
003600030210     c                   Iter
003700030210     c                   EndIf
003800030210      * Converto spese incasso
003900030211     c     IndSin        Div(h)    1936,27       w0052
004000030211     c                   z-add     w0052         w0053
004100030210     c                   move      w0053         IndSin
004200030210      * Aggiorno Tfind00f
004300030210     c                   Update    Cnind000
004400021216     c                   EndDo
004500021216
004600021216     c                   Eval      *InLr = *On
