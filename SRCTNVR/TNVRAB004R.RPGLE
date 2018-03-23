000100940224     H*--------------------------------------------------------------*
000200160628      **** sistema le CAUSALI ORIGINALI DPD
000300160628     H*--------------------------------------------------------------*
000400160623     Ftnecf01L  uf   E           k DISK
000500160628     Ftnefr02L  if   E           k DISK
000600900131     C*---------------------------------------------------------------*
000700160628     c     Kefr02        klist
000800160628     c                   kfld                    DPD
000900160628     c                   kfld                    ecfrif
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
002100160628     c     Kefr02        setll     tnefr02l
002200160628     c     Kefr02        reade     tnefr02l
002300160628     c                   Dow       not %EoF(tnefr02L)
002400160628      *
002500160628      *  Causale sui primi 3 del campo Filler
002600160628     c                   if        %subst(ecfflr:1:3)= *blank
002700160628     c                   eval       %subst(ecfflr:1:3)= efrcau
002800160628     c                   else
002900160628      * Forza se ORM o RESO o ISOLA
003000160628     c                   if        %subst(efrcau:1:1)= 'O' or
003100160628     c                             %subst(efrcau:1:1)= 'R' or
003200160628     c                             %subst(efrcau:3:1)= 'I'
003300160628     c                   eval       %subst(ecfflr:1:3)= efrcau
003400160628     c                   end
003500160628     c                   end
003600160628      *
003700160628     c     Kefr02        reade     tnefr02l
003800160628     c                   enddo
003900100716     C*
004000160623     c                   update    tnecf000
004100030205     c                   enddo
004200000616     C*
004300000000     C                   SETON                                        LR
