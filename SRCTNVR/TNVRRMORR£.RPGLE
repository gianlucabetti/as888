000100940224     H*--------------------------------------------------------------*
000200060421     Fazorg01L  UF   E           k DISK
000300160623     FazorgSED  uF   E           K DISK    usropn extfile(worgsed)
000400160623     F                                     RENAME(azorg:azorgSD)
000500060421     d og143         e ds
000600160623     d og150         e ds
000700160623     d worgsed         s             21    inz('GAITRAGRU /AZORG01L')
000800900131     C*---------------------------------------------------------------*
000900030205      *
001000160623    0c                   do        *hival
001100060421     c                   read      azorg01l
001200030515     c* fine file
001300160623    1c                   if        %eoF(azorg01l)
001400030515     c                   leave
001500160623    1c                   end
001600030205     C*
001700060421     c                   movel     orgde3        og143
001800160623    1c                   if        §ogntw='COR' or §ogntw='MES'
001900160623     c                   movel     orgdf0        og150
002000160623     c
002100160623     c                   eval      §ogagspus='S'
002200160623     c                   eval      orgdf0= og150
002300160623     c
002400160623     c                   add       1             conta             4 0
002500160623     c                   update    azorg
002600160623    1c                   endif
002700041122     C*
002800160623    0c                   enddo
002900160623     c
003000160623     c                   open(e)   azorgsed
003100160623     c                   if        %error
003200160623     c                   eval      %subst(worgsed:7:4)='GRPS'
003300160623     c                   open(e)   azorgsed
003400160623     c                   endif
003500160623     c
003600160623    0c                   do        *hival
003700160623     c                   read      azorgsed
003800160623     c* fine file
003900160623    1c                   if        %eoF(azorgsed)
004000160623     c                   leave
004100160623    1c                   end
004200160623     C*
004300160623     c                   movel     orgde3        og143
004400160623    1c                   if        §ogntw='COR' or §ogntw='MES'
004500160623     c                   movel     orgdf0        og150
004600160623     c
004700160623     c                   eval      §ogagspus='S'
004800160623     c                   eval      orgdf0= og150
004900160623     c
005000160623     c                   add       1             contasd           4 0
005100160623     c                   update    azorgsd
005200160623    1c                   endif
005300160623     C*
005400160623    0c                   enddo
005500160623     c
005600160623     c                   dsply                   conta
005700160623     c                   dsply                   contasd
005800000616     C*
005900000000     C                   SETON                                        LR
