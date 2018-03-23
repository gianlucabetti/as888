000100940224     H*--------------------------------------------------------------*
000200160623     Ftnecf01L  uf   E           k DISK
000300160623     Ftitas30c  if   E           k DISK
000400900131     C*---------------------------------------------------------------*
000500160623     c     bolla         klist
000600160623     c                   kfld                    ecfaas
000700160623     c                   kfld                    ecflnp
000800160623     c                   kfld                    ecfnrs
000900160623     c                   kfld                    ecfnsp
001000160623      *
001100160623     c                   movel     'DPD'         DPD               3
001200160623     c     DPD           setll     tnecf01l
001300030515     c*
001400030515     c                   do        *hival
001500160623     c     DPD           reade     tnecf01l
001600030515     c* fine file
001700160623     c                   if        %eoF(tnecf01l)
001800030515     c                   leave
001900030515     c                   end
002000100716     C*
002100160623     c     bolla         chain     titas30c
002200160623     c                   if        %Found(titas30c)
002300160720     c***************    if        ecfnaz=*blank and ecfcts = *blank
002400030205     C*
002500160623     c                   if        ecfLNA >=190 and ecfLNA<=197 or
002600160623     c                             ecfLNA >=291 and ecfLNA<=293
002700160720     c                   if         tasNZD <> *blank
002800160623     c                   eval      ecfNAZ    = tasNZD
002900160623     c                   eval      ecfCTS    = tasCTS
003000160623     C*
003100160623     c                   end
003200160720     c                   end
003300160623     C*
003400160623     c                   if        ecfLNp >=190 and ecfLNp<=197 or
003500160623     c                             ecfLNp >=291 and ecfLNp<=293
003600160720     c                   if         tasNZM <> *blank
003700160623     c                   eval      ecfNAZ    = tasNZM
003800160720     c                   eval      ecfCTS    = tasMCT
003900160623     C*
004000160720     c                   end
004100160623     c                   end
004200160720     c***************    end
004300160623     c                   end
004400100716     C*
004500160623     c                   update    tnecf000
004600030205     c                   enddo
004700000616     C*
004800000000     C                   SETON                                        LR
