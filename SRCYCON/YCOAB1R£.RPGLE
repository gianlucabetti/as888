000100080211     H DFTACTGRP(*NO) BNDDIR('PRNPGM')
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400990506     Fanact01l  uf a e           k disk
000500990506     Fanacr01l  uf a e           k disk
000600990506     d dscnabi       e ds                  extname(cnabi00f)
000700000000     C*---------------------------------------------------------------*
000800981230     c     *entry        plist
000900981230     c                   parm                    dscnabi
001000981230      *
001100990506     c                   move      abiabi        kabi
001200990506     c                   move      abicab        Kcab
001300990506     C     kact          klist
001400990506     C                   kfld                    kabi
001500990506     C     kacr          klist
001600990506     C                   kfld                    Kabi
001700990506     C                   kfld                    Kcab
001800990506     C* Definizioni variabili
001900990506     C     *like         define    actabi        kabi
002000990506     C     *like         define    acrcab        kcab
002100981230      *
002200990506      *  Eseguo scrittura/Aggiornamento ANACT
002300990506     c     kact          chain     anact01l                           99
002400990506     c   99              clear                   anact000
002500990506     c   99              move      abiabi        actabi
002600990506     c                   movel     abiist        actdesist
002700990506     c                   move      abiist        wcomo36          36
002800990506     c                   movel     wcomo36       actdesagis
002900990506     c                   movel     abiist        actdesbris
003000080211     c                   IF        *IN99
003100000321     C                   MOVE      ABICOM        WSTATO            4
003200000321     c                   if        Wstato=*blank
003300990528     c                   movel     'IT  '        actstato
003400000321     c                   else
003500000321     c                   movel     wstato        actstato
003600000321     c                   endif
003700080211     c                   ENDIF
003800990506     c  n99              update    anact000
003900990506     c   99              write     anact000
004000000321      *
004100990506     c     kacr          chain     anacr01l                           99
004200990506     c   99              clear                   anacr000
004300990506     c   99              move      abiabi        acrabi
004400990506     c   99              move      abicab        acrcab
004500130910      * se anacr già annullato esco
004600130910     c                   if        %SUBST(acrDesBrAg : 1 : 9) = 'ANNULLATO'
004700130910     c                   eval      *inlr = *on
004800130910     c                   return
004900130910     c                   endif
005000130910     c
005100990506     c                   movel     abiage        acrdesage
005200130910     c                   if        abiann = '*'
005300080326     c                   eval      acrDesBrAg = 'ANNULLATO ' + %CHAR(%DATE())
005400080319     c                   else
005500080319     c                   movel     abiage        acrdesbrag
005600080319     c                   endif
005700990506     c                   movel     abind         acrindiriz
005800990506     c                   move      abind         wcomo36
005900990506     c                   movel     wcomo36       acrindagg
006000000321     c                   movel     abicom        wcomo36
006100000321>>>>>C                   MOVEL     wcomo36       ACRLocalit
006200990506>>>>>C                   If        ABILoc <> *Blank
006300000321>>>>>C                             and abiloc <> wcomo36
006400990506>>>>>C     acrlocalit    cat       ABILoc:1      ACRLocalit
006500990506>>>>>C                   EndIf
006600990506     c                   movel     abicap        acrcap
006700990506     c                   movel     abipro        acrprov
006800000321     C                   MOVE      ABICOM        WSTATO            4
006900000321     c                   if        Wstato=*blank
007000000321     c                   movel     'IT  '        acrstato
007100000321     c                   else
007200000321     c                   movel     wstato        acrstato
007300000321     c                   endif
007400000321     c*****              movel     'IT  '        acrstato
007500080319     c                   IF        NOT *IN99
007600080319     c                   update    anacr000
007700080319     c                   else
007800080319     c                   write     anacr000
007900080319     c                   ENDIF
008000981230      *
008100981029     c                   seton                                        lr
