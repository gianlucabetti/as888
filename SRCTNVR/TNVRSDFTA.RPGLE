000100021016     h Decedit('0,') Datedit(*YMD.) option(*nodebugio)
000200021016
000300021017     fTntbe01l  uf   e           k Disk
000400021016
000500021016      * Ds date statistiche
000600021016     d dSdf          e ds                  inz
000700021016
000800021016     d Ut§dse0f      e ds
000900021016     d Cncr80        e ds
001000021016     d Kpjba         e ds
001100021016
001200021016     c     Ktbe          Klist
001300021016     c                   Kfld                    TbeCod
001400021016     c                   Kfld                    TbeKe1
001500021016
001600021017     c     *DTAARA       DEFINE                  TNTA66            6 0
001700021017     c                   IN        TNTA66
001800021016
001900021016      * Reperimento parametri aziendali
002000021016     c                   Z-Add     1             CODUT
002100021016     c                   CALL      'X§PARUT'
002200021016     c                   PARM                    Ut§dse0f
002300021016     c                   MOVEL     REC80         CNCR80
002400021016
002500021016     c                   Eval      TbeCod = 'SDF'
002600021016     c                   Movel(p)  Simfel        TbeKe1
002700021016     c     Ktbe          Chain     Tntbe01l
002800021017     c                   If        %Found(Tntbe01l)
002900021017     c                   Movel     TbeUni        dSdf
003000021017     c                   Z-Add     Tnta66        §SdfStcU
003100021016     c                   Movel     dSdf          TbeUni
003200021017     c                   Update    Tntbe000
003300021016     c                   EndIf
003400021016
003500021016     c                   Eval      *InLr = *On
