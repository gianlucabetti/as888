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
002000020304      * per giornata
002100020304     c                   move      'G'           ktsr
002200011026     c                   exsr      fifgt
002300020304      * per consegna
002400020304     c                   move      'C'           ktsr
002500020304     c                   exsr      fifgt
002600020304      * per ritiro
002700020304     c                   move      'R'           ktsr
002800020304     c                   exsr      fifgt
002900020308
003000020308     c                   endsr
003100020304
003200990113     c*-------------------------------------------------
003300011026     c     fifgt         begsr
003400990113     c*-------------------------------------------------
003500990113     c                   setoff                                           99
003600020305     c     kfgt          setll     fifgt03l
003700981029     c                   do        *hival
003800020305     c     kfgt          reade     fifgt03l                               99
003900981029     c   99              leave
004000020304      * non considero le tariffe che risultano fuori periodo
004100020403     c                   if        fgtddt > alladat or  fgtdst < alladat or
004200020403     c                             fgtdts = 0
004300020114     c                   iter
004400020114     c                   end
004500050503     c                   if        fgtdcn = 0
004600020308     c                   move      dataoggi      fgtdcn
004700050503     c                   end
004800020308     c                   update    fifgt000
004900020304     c                   enddo
005000020304
005100020304     c                   endsr
005200981029      *
005300011026     c*-------------------------------------------------
005400011026     c     *inzsr        begsr
005500011026     c*-------------------------------------------------
005600020304     c     *entry        plist
005700020304     c                   parm                    kpjba
005800020304     c                   movel     kpjbu         param
005900020304
006000020304     c                   z-add     codpadr       kpdr
006100020304     c
006200020304     c     kfgt          klist
006300020403     c                   kfld                    kpdr              7 0
006400020403     c                   kfld                    ksml              1
006500020403     c                   kfld                    ktsr              1
006600020403     c                   clear                   ksml
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
