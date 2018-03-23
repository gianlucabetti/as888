000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300140324     ffiar501l  uf   e           k disk    rename(fiar5000:fiar5) prefix(x)
000400140324     ffiar531c  if   e           k disk
000401140324     d dar5gen       e ds
000402140324     d xdar5gen      e ds                  extname(dar5gen) prefix(x)
000403140324     D Cmd1            S             48    dim(1) ctdata perrcd(1)              QCAEXEC
000404140324     D Cmd             S             48                                         QCAEXEC
000800000607      *--------------------------------------------------------------------
000900040409      * lettura
001000140324     c     *loval        setll     fiar501l
001100000607     c                   do        *hival
001200140324     c                   read      fiar501l
001300140324     c                   if        %eof(fiar501l)
001400000607     c                   leave
001500000607     c                   end
001501140324     c                   if        xar5trd <> 'GEN'
001502140324     c                   iter
001503140324     c                   end
001504140324     c                   move      xar5uni       xdar5gen
001505140324     c                   if        xdar5gen = *blank  or
001506140324     c                             %subst(xar5uni:1:144) = *blank and
001507140324     c                             %subst(xar5uni:145:2) = 'NO' and
001508140324     c                             %subst(xar5uni:147:54) = *blank
002000140324     c     kar531        chain     fiar531c
002100140324     c                   if        %found(fiar531c)
002200140324     c                   move      ar5uni        dar5gen
002201140324     c                   move      x§ar5pdaco    §ar5pdaco
002202140324     c                   movel     dar5gen       ar5uni
002203140324     c                   movel     ar5uni        xar5uni
002204140324     c                   clear                   xar5ft2
002205140324     c                   clear                   xar5dt2
002206140324     c                   update    fiar5
002300051130     c                   end
002301140324     c                   end
006600000607
006700051130     c                   enddo
006800000607     c                   seton                                        lr
006900140218      *-----------------------------------------------
007000140218     c     *inzsr        begsr
007100140218      *-----------------------------------------------
007200140218     c
008500140324     c     kar531        klist
008600140324     c                   kfld                    xAR5AAS
008700140324     c                   kfld                    xAR5LNP
008800140324     c                   kfld                    xAR5NRS
008900140324     c                   kfld                    xAR5NSP
008901140324     c                   kfld                    wTRD
008905140324     c                   move      'GEN'         wtrd              3
008906140324      *
008907140324     C                   movea     cmd1          cmd
008910140324     C                   movel(p)  cmd           comman
008911140324     C                   call      'QCMDEXC'
008912140324     C                   parm                    comman           80
008913140324     C                   parm      48            lung             15 5
009100140218     c                   endsr
009200140324**         CMD1
009300140324OVRDBF FILE(FIAR501L) TOFILE(FILTRA201/FIAR501L)
