000200090109     H DEBUG DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
000300000000     H*--------------------------------------------------------------*
000400020305     h* OVRPRTF FILE(QSYSPRT) LPI(6) CPI(15) PAGRTT(0)
000502090325     Ffitgt01l  uf   e           k disk
000600020304     dkpjba          e ds
000700020304     dparam            ds
000800020304     d codpadr                        7s 0
000900020304     d alladat                        8s 0
001000030416     d serv1                          1
001001090407     d esito                          1
001002090417     d dtadrc                         8s 0
001100020305     d dataiso         s               d   datfmt(*iso)
001200020305     d dataeur         s               d   datfmt(*eur)
001300000000     C*---------------------------------------------------------------*
001600020308     c                   seton                                        lr
001601090407     c                   eval      esito = '1'
001904090109     c* cerco il progressivo
001905090109     c     ktgt          setll     fitgt01l
001906090109     c                   do        *hival
001907090109     c     ktgt          reade     fitgt01l
001908090109     c                   if        %eof(fitgt01l)
001909090109     c                   leave
001910090109     c                   end
001911090109     c* escludo le annullate
001912090109     c                   if        tgtatb <> ' '
001913090109     c                   iter
001914090109     c                   end
001915090109      * non considero le tariffe che risultano fuori periodo
001920090109     c                   if        tgtddt > alladat or  tgtdst < alladat
001921090325     c                             or tgtdts = 0 or tgtdcn = 0
001922090109     c                   iter
001923090109     c                   end
001924090407     c                   eval      esito = ' '
001925090417     c                   if        dtadrc <> 0
001926090417     c                   move      dtadrc        tgtdrc
001927090417     c                   else
002806090325     c                   move      dataoggi      tgtdrc
002807090417     c                   end
002808090112     c                   update    fitgt000
002809090325     c                   enddo
002810090112     c*
002811090109     c                   commit
002812090420     c                   movel     param         kpjbu
005300011026     c*-------------------------------------------------
005400011026     c     *inzsr        begsr
005500011026     c*-------------------------------------------------
005600020304     c     *entry        plist
005700020304     c                   parm                    kpjba
005800020304     c                   movel     kpjbu         param
005900020304
006000020304     c                   z-add     codpadr       kpdr
006100020304     c
006601090109     c     ktgt          klist
006602090325     c                   kfld                    kpdr              7 0
006603090325     c                   kfld                    ksml              1
006700011026
006800020218     C                   time                    w0120            14 0
006900020218     C                   move      w0120         wdat              8 0
007000020308      * data da impostare su data stampa e convalida se lancio = 'C'
007100020308     c                   move      wdat          dataeur
007200020308     c                   move      dataeur       dataiso
007300020308     c                   move      dataiso       dataoggi          8 0
007400020308      * data da elaborare
007500020304     c                   move      alladat       dataiso
007600020304     c                   move      dataiso       dataeur
007700020304     c                   move      dataeur       alladata          8 0
007800020313
007900011026     c                   endsr
