000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060125      *****************************************************************
000300060125      *
000400060125      *****************************************************************
000500161107     Fftt_sav   IF   E             DISK    rename(ftt_sav:fttsav)
000600161107     Ffifce01l  uf   E           k DISK
000700030429     F*---------------------------------------------------------------------------------
000800000607     D kpjba         e ds
000900020205     c**********************************************************************
001000161107     c     kfce          klist
001100161107     c                   kfld                    fgs               3 0
001200161107     c                   kfld                    pdr               7 0
001300030429      *
001400161104     c     *start        setll     ftt_sav
001500161026     c                   do        *hival
001600161104     c                   read      ftt_sav
001700161104     c                   if        %eof(ftt_sav)
001800161026     c                   leave
001900161026     c                   endif
002000161107     c                   movel     Fttpdr        fgs
002100161107     c                   z-add     FttPDR        pdr
002200161107     c     kfce          setll     fifce01l
002300161107     c                   do        *hival
002400161107     c     kfce          reade     fifce01l
002500161107     c                   if        %eof(fifce01l)
002600161107     c                   leave
002700161107     c                   endif
002800161107     c                   if        fttddc = fceddc
002900161107     c                   add       freimp        fceice
003000161107     c                   update    fifce000
003100161107     c                   leave
003200161107     c                   else
003300161107     c                   iter
003400161107     c                   endif
003500161107     c                   enddo
003600161107      *
003700161026     c                   enddo
003800000607     c                   seton                                        lr
003900020205     C**-------------------------------------------------------------**
004000000607     c     *inzsr        begsr
004100060125     C**-------------------------------------------------------------**
004200000607     c     *entry        plist
004300020205     c                   parm                    kpjba
004400020402     c*
004500020402     c                   endsr
