000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300020318     ftntbe01l  if   e           k disk
000400020318     D ficn70ds      e ds
000500020318     d kcod            s                   like(tbecod)
000600020318     d key1            s                   like(tbeke1)
000700020318     d key2            s                   like(tbeke2)
000800040309     D* DS PER TRUL06R - CARICAMENTO £1
000900040309     D kpjba         E DS
001000040309     D savkpjbu        s                   like(kpjbu)
001100040309     D DSUL06        E DS                  EXTNAME(TRUL06DS)
001200040309     D  LIN                    1     90  0
001300040309     D                                     DIM(30)                              SKI COMODO
001400080812     D TRUL72DS      E DS
001500080812     D  L72                           3s 0 DESCEND
001600080812     D                                     DIM(60)
001700080812     D                                     OVERLAY(O72Lia)
001800000607      *--------------------------------------------------------------------
001900000607     c
002000000607     c     *entry        plist
002100020318     c                   parm                    ficn70ds
002200040309      * caricamento £1 verifica se po dell'area serve solo in caso di key2
002300040309      * = a *blank
002400040309     c                   movel(p)  kpjbu         savkpjbu
002500080812     c                   if        i70tp£ <> 'C'
002600040309     C                   CLEAR                   DSUL06
002700040309     c                   move      '£1'          d06cod
002800040309     C                   MOVEL     i70pog        d06key
002900040309     C                   MOVEL     DSUL06        KPJBU
003000040309     C*
003100040309     C                   CALL      'TRUL06R'
003200040309     C                   PARM                    KPJBA
003300040309     C                   MOVEL     KPJBU         DSUL06
003400080812     c                   else
003500080812     c                   clear                   trul72ds
003600080812     c                   eval      i72tla = 'L'
003700080812     c                   eval      I72POU = i70pog
003800080812     c                   eval      kpjbu = trul72ds
003900080812     C                   CALL      'TRUL72R'
004000080812     C                   PARM                    KPJBA
004100080812     c                   movel     kpjbu         trul72ds
004200080812     c                   endif
004300040309     c                   movel(p)  savkpjbu      kpjbu
004301090114     c                   clear                   o70fil
004400020319     c* se sono utente EDP sono abilitato a tutto
004500020319     c                   if        %subst(i70prf: 1: 3) = 'EDP'
004600020319     c                   movel     *all'S'       o70uni
004700020319     c                   else
004800020319     c*
004900020318     c                   move      i70pog        i70poga           3
005000020318     c                   move      i70pge        i70pgea           3
005100020318     c
005200020318
005300020318     c     ktbe          klist
005400020318     c                   kfld                    kcod
005500020318     c                   kfld                    key1
005600020318     c                   kfld                    key2
005700020318     c                   eval      kcod = 'AUT'
005800000607
005900020318      *cerco con chiave completa (PO in chiave 2)
006000020318     c                   eval      key1 = i70poga + i70prf
006100020318     c                   eval      key2 = i70pgea
006200020318     c     ktbe          chain     tntbe01l                           99
006300020318     c                   if        not *in99
006400020318     c                   movel     tbeuni        o70uni
006500020318     c                   else
006600020318      *cerco con chiave incompleta (blank in chiave 2)
006700020318     c                   eval      key2 = *blank
006800040309      * con key2 = *blank verifica se la filiale da gestire è in £1
006900040312     c                   if        i70pog =  046
007000040312     c                   seton                                        98
007100040312     c                   else
007200080812     c                   if        i70tp£ <> 'C'
007300040312     c     i70pge        lookup    lin                                    98
007400080812     c                   else
007500080812     c     i70pge        lookup    l72                                    98
007600080812     c                   endif
007700040312     c                   endif
007800020318     c     ktbe          chain     tntbe01l                           99
007900040309     c                   if        not *in99 and *in98
008000020318     c                   movel     tbeuni        o70uni
008100020318     c                   else
008200020318     c                   move      *all'N'       o70uni
008300020318     c                   end
008401090504     c* se pge (filiale da gestire) non è gestita ritorno il flag di errore
008402090504     c                   if        not *in98
008403090504     c                   eval      o70fil = 'N'
008404090504     c                   else
008405090504     c                   clear                   o70fil
008406090504     c                   end
008407090504     c                   end
008500020319     c                   end
008600020318
008700000607     c                   seton                                        lr
