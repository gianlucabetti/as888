000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400020305     h* OVRPRTF FILE(QSYSPRT) LPI(6) CPI(15) PAGRTT(0)
000500020308     Ffifgt03l  uf   e           k disk
000600020304     dkpjba          e ds
000700020304     dparam            ds
000800020304     d codpadr                        7s 0
000900020304     d alladat                        8s 0
001000030416     d serv1                          1
001100020305     d dataiso         s               d   datfmt(*iso)
001200020305     d dataeur         s               d   datfmt(*eur)
001300000000     C*---------------------------------------------------------------*
001400020304      *
001500020403     c                   exsr      elabora
001600020308     c                   seton                                        lr
001700020308      *--------------------------------------------------
001800020308     c     elabora       begsr
001900020308      *--------------------------------------------------
002000030203      * per coop
002100030416     c                   move      serv1         ktsr
002200011026     c                   exsr      fifgt
002300020308
002400020308     c                   endsr
002500020304
002600990113     c*-------------------------------------------------
002700011026     c     fifgt         begsr
002800990113     c*-------------------------------------------------
002900990113     c                   setoff                                           99
003000020305     c     kfgt          setll     fifgt03l
003100981029     c                   do        *hival
003200020305     c     kfgt          reade     fifgt03l                               99
003300981029     c   99              leave
003400020304      * non considero le tariffe che risultano fuori periodo
003500020403     c                   if        fgtddt > alladat or  fgtdst < alladat or
003600020403     c                             fgtdts = 0
003700020114     c                   iter
003800020114     c                   end
003900050503     c                   if        fgtdcn = 0
004000020308     c                   move      dataoggi      fgtdcn
004100050503     c                   end
004200020308     c                   update    fifgt000
004300020304     c                   enddo
004400020304
004500020304     c                   endsr
004600981029      *
004700011026     c*-------------------------------------------------
004800011026     c     *inzsr        begsr
004900011026     c*-------------------------------------------------
005000020304     c     *entry        plist
005100020304     c                   parm                    kpjba
005200020304     c                   movel     kpjbu         param
005300020304
005400020304     c                   z-add     codpadr       kpdr
005500020304     c
005600020304     c     kfgt          klist
005700020403     c                   kfld                    kpdr              7 0
005800020403     c                   kfld                    ksml              1
005900020403     c                   kfld                    ktsr              1
006000020403     c                   clear                   ksml
006100011026
006200020218     C                   time                    w0120            14 0
006300020218     C                   move      w0120         wdat              8 0
006400020308      * data da impostare su data stampa e convalida se lancio = 'C'
006500020308     c                   move      wdat          dataeur
006600020308     c                   move      dataeur       dataiso
006700020308     c                   move      dataiso       dataoggi          8 0
006800020308      * data da elaborare
006900020304     c                   move      alladat       dataiso
007000020304     c                   move      dataiso       dataeur
007100020304     c                   move      dataeur       alladata          8 0
007200020313
007300011026     c                   endsr
