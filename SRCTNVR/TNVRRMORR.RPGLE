000001170228     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000100940224     H*--------------------------------------------------------------*
000200060421     Fazorg01L  UF   E           k DISK
000201170228     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000600160623     d og150         e ds
000601170228     d fnlv55ds      e ds
000800900131     C*---------------------------------------------------------------*
000900030205      *
000901170228     c     *loval        setll     azorg01l
000902170228     c                   read      azorg01l
001000170228    0c                   dow       not %eof(azorg01l)
001001170228    1c                   if        orgfag='F' or orgfag='A'
001600030205     C*
001601170228     c                   clear                   fnlv55ds
001602170228     c                   eval      d55lin=orgfil
001603170228     c                   eval      d55drf=*date
001604170228     c                   eval      d55tpt='P'
001605170228     c                   call      'FNLV55R'
001606170228     c                   parm                    fnlv55ds
001900160623     c                   movel     orgdf0        og150
001901170228    2c                   if        d55err=*blanks and orgfil=d55tfp
002000170228     c                                            and §ogdkc='S'
002100170228     c                   eval      §ogdkcdir='S'
002200160623     c                   eval      orgdf0= og150
002300160623     c
002500160623     c                   update    azorg
002501170228     c                   except    riga
002700041122     C*
002701170228    2c                   endif
002702170228    1c                   endif
002703170228     c                   read      azorg01l
002800160623    0c                   enddo
005800000616     C*
005900000000     C                   SETON                                        LR
006000170228     OQSYSPRT   E            riga        1
006200170228     O                       orgfil            +  1
