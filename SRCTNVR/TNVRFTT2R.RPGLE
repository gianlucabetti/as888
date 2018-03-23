000100060207     h Decedit('0,') Datedit(*YMD.)
000200001006
000300040225
000400060207     fdistinte  if   e             Disk    rename(distinte:fttval00)
000500060207     fFiftt01l  uf   e           k Disk
000600060207     fFifce01l  uf   e           k Disk
000700060208     fFiftd01l  uf   e           k Disk    prefix(£)
000800010116
000900001006      *------------------------------------------------------------------------*
001000060207     c     kftt          klist
001100060207     c                   kfld                    fttpdr
001200060207     c                   kfld                    ftttsr
001300060207     c                   kfld                    fttndc
001400060207     c                   kfld                    fttddc
001500060207     c     kftt1         klist
001600060207     c                   kfld                    fcefgs
001700060207     c                   kfld                    fttpdr
001800060207     c                   kfld                    ftttsr
001900060207     c                   kfld                    fttndc
002000060207     c                   kfld                    fttddc
002100040225
002200040225do  1c                   Do        *Hival
002300060207     c                   Read      distinte
002400040226
002500060207     c                   If        %eof(distinte)
002600040226     c                   Leave
002700040226     c                   EndIf
002800060207     c                   z-add     ftdpdr        fttpdr
002900060207     c                   move      'C'           ftttsr
003000060207     c                   z-add     ftdndc        fttndc
003100060207     c                   z-add     ftdddc        fttddc
003200060207      * solo ftt valorizzati e non confermati
003300060207     c     kftt          chain     fiftt01l
003400060207     c                   If        %found(fiftt01l)
003500060207     c                   eval      fttfvl = ' '
003600060207     c                   eval      fttftr = ' '
003700060207     c                   eval      fttitt = 0
003800060207     c                   eval      fttita = 0
003900060207     c                   eval      ftttpe = 0
004000060207     c                   eval      ftttbn = 0
004100060207     c                   eval      ftttim = 0
004200060207     c                   eval      fttmnt = 0
004300060207     c                   eval      fttdcv = 0
004400060207     c                   update    fiftt000
004500060207     c                   EndIf
004600060207     c*
004700060207     c                   movel     fttpdr        fcefgs
004800060207     c     kftt1         setll     fifce01l
004900060207     c                   do        *hival
005000060207     c     kftt1         reade     fifce01l
005100060207     c                   If        %eof(fifce01l)
005200060207     c                   Leave
005300060207     c                   EndIf
005400060207     c                   delete    fifce000
005500060207     c                   enddo
005600060208     c*
005700060208     c     kftt          setll     fiftd01l
005800060208     c                   do        *hival
005900060208     c     kftt          reade     fiftd01l
006000060208     c                   If        %eof(fiftd01l)
006100060208     c                   Leave
006200060208     c                   EndIf
006300060208     c                   eval      £ftditt = 0
006400060208     c                   eval      £ftdita = 0
006500060208     c                   update    fiftd000
006600060208     c                   enddo
006700060207     c*************************************************
006800060207     c                   z-add     ftdpdr        fttpdr
006900060207     c                   move      'R'           ftttsr
007000060207     c                   z-add     ftdndc        fttndc
007100060207     c                   z-add     ftdddc        fttddc
007200060207      * solo ftt valorizzati e non confermati
007300060207     c     kftt          chain     fiftt01l
007400060207     c                   If        %found(fiftt01l)
007500060207     c                   eval      fttfvl = ' '
007600060207     c                   eval      fttftr = ' '
007700060207     c                   eval      fttitt = 0
007800060207     c                   eval      fttita = 0
007900060207     c                   eval      ftttpe = 0
008000060207     c                   eval      ftttbn = 0
008100060207     c                   eval      ftttim = 0
008200060207     c                   eval      fttmnt = 0
008300060207     c                   eval      fttdcv = 0
008400060207     c                   update    fiftt000
008500060207     c                   EndIf
008600060207     c*
008700060207     c                   movel     fttpdr        fcefgs
008800060207     c     kftt1         setll     fifce01l
008900060207     c                   do        *hival
009000060207     c     kftt1         reade     fifce01l
009100060207     c                   If        %eof(fifce01l)
009200060207     c                   Leave
009300060207     c                   EndIf
009400060207     c                   delete    fifce000
009500060207     c                   enddo
009600060208     c*
009700060208     c     kftt          setll     fiftd01l
009800060208     c                   do        *hival
009900060208     c     kftt          reade     fiftd01l
010000060208     c                   If        %eof(fiftd01l)
010100060208     c                   Leave
010200060208     c                   EndIf
010300060208     c                   eval      £ftditt = 0
010400060208     c                   eval      £ftdita = 0
010500060208     c                   update    fiftd000
010600060208     c                   enddo
010700060207     c******************************************************
010800060207     c                   z-add     ftdpdr        fttpdr
010900060207     c                   move      ' '           ftttsr
011000060207     c                   z-add     0             fttndc
011100060207     c                   z-add     ftdddc        fttddc
011200060207      * solo ftt valorizzati e non confermati
011300060207     c     kftt          chain     fiftt01l
011400060207     c                   If        %found(fiftt01l)
011500060207     c                   delete    fiftt000
011600060207     c                   EndIf
011700040225
011800040225     c                   EndDo
011900010202
012000040225     c                   Eval      *Inlr = *On
