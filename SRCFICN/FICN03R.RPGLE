000101081229     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000102081229     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300000000     H*--------------------------------------------------------------*
000400090331     Ffifgt01l  uf   e           k disk    commit(flgcmt) usropn
000500081229     Ffitgt01l  if   e           k disk
000800000000     C*---------------------------------------------------------------*
000900020109      *conversioni
001500081229     c     ktgt          chain     fitgt01l                           99
001501081229     c     kfgt          setll     fifgt01l
001502081229     c                   do        *hival
001503081229     c     kfgt          reade     fifgt01l
001504081229     c                   if        %eof(fifgt01l)
001505081229     c                   leave
001506081229     c                   end
001700081229     c                   if        fgtatb = *blank and fgtprg = tgtprg
001701081229     c                   eval      fgtddt = tgtddt
001702081229     c                   eval      fgtdst = tgtdst
001703090424     c                   eval      fgtdcn = tgtdrc
001704081229     c                   eval      fgtdts = tgtdts
001705081229     c                   eval      fgtduv = TGTDUV
001706090116     c*                  eval      fgtdcv = TGTDCV
002300020211     c                   update    fifgt000
002400020114     c                   end
002401081229     c                   enddo
005500020211
005501081229     c                   seton                                        lr
005700011026     c*-------------------------------------------------
005800011026     c     *inzsr        begsr
005900011026     c*-------------------------------------------------
006000020211     c     *entry        plist
006100020211     c                   parm                    pdr               7 0
006200020211     c                   parm                    sml               1
006500020211     c                   parm                    prg               3 0
006501090331     c                   parm                    comcom            1
006502090331     c                   if        comcom = '1'
006503090331     c                   eval      flgcmt = *on
006504090331     c                   else
006505090331     c                   eval      flgcmt = *off
006506090331     c                   end
006507090331     c                   open      fifgt01l
006600020211     c     kfgt          klist
006700020211     c                   kfld                    pdr
006800020211     c                   kfld                    sml
007200011026
007300081229     c     ktgt          klist
007400081229     c                   kfld                    pdr
007500081229     c                   kfld                    sml
007600081229     c                   kfld                    prg
008000011026
008100011026     c                   endsr
008200011129     C*--------------------------------------------------------------------
