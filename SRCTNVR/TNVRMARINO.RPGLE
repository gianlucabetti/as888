000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300080428     Fcnind00f  uF   E           K DISK
000400080428     Fcnaco00f  uF   E           K DISK
000500080422     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000600080428     d
000700080428     dwpos             s              2  0
000800080428     dleft             s              2  0
000900080428     dw007a            s              7
001000021214      *--------------------------------------------------
001100080428     c     kind          klist
001200080428     c                   kfld                    codut             1 0
001300080428     c                   kfld                    kcc               4 0
001400080428     c     kaco          klist
001500080428     c                   kfld                    indkut
001600080428     c                   kfld                    indkcc
001700080428     c                   kfld                    indksc
001800080428     c
001900080428     c                   z-add     1             codut
002000080428     c                   z-add     151           kcc
002100080428     c                   setoff                                       30
002200080428     c
002300080428     c     kind          setll     cnind00f
002400080428     c     kind          reade(N)  cnind00f
002500051110
002600080428    1c                   DOW       not %EOF(cnind00f)
002700080428     c                   if        indsta  = *Blanks
002800080428      * provincia FO o RN
002900080428     c                             and (indprv  = 'FO' or indprv  = 'RN'
003000080428     c                                  or indprv  = 'FC')
003100080428      * controllo se nella partita iva c'è la sigla SM x San Marino
003200080428     c                   Clear                   wpos
003300080428     c                   Eval      wpos = %scan('SM':indiva)
003400080428      * San Marino
003500080428if  3c                   If        wpos > *Zeros
003600080428     c
003700080428     c     kaco          chain     cnaco00f
003800080428     c     kaco          chain     cnind00f
003900080428     c
004000080428     c                   select
004100080428     c                   when      wpos=1 and %subst(indiva:3:1)=' '
004200080428     c
004300080428     c                   eval      left=  %check('SM ':indiva)
004400080428     c                   eval      w007a='SM'+%subst(indiva:left:5)
004500080428     c                   except    ivaerr
004600080428     c                   clear                   indiva
004700080428     c                   eval      indiva=w007a
004800080428     c                   Clear                   acoftr
004900080428     c
005000080428     c                   when      wpos>1 and %subst(indiva:(wpos+2):1)<>' '
005100080428     c                   eval      w007a=%subst(indiva:wpos:7)
005200080428     c                   except    ivaerr
005300080428     c                   clear                   indiva
005400080428     c                   eval      indiva=w007a
005500080428     c                   Clear                   acoftr
005600080428     c
005700080428     c                   when      wpos>1 and %subst(indiva:(wpos-1):1)<>' '
005800080428     c                   eval      w007a='SM'+%subst(indiva:(wpos-5):5)
005900080428     c                   except    ivaerr
006000080428     c                   clear                   indiva
006100080428     c                   eval      indiva=w007a
006200080428     c                   Clear                   acoftr
006300080428     c                   endsl
006400080428     c
006500080428     c                   if        indcdf<>*blanks
006600080428     c                   except    cdfcanc
006700080428     c                   clear                   indcdf
006800080428     c                   Clear                   acoftr
006900080428     c                   endif
007000080428     c
007100080428     c                   if        acoftr=' '
007200080428     c                   Clear                   acodtr
007300080428     c
007400080428     c                   update    cnind000
007500080428     c                   update    cnaco000
007600080428     c
007700080428     c                   else
007800080428     c                   unlock    cnind00f
007900080428     c                   unlock    cnaco00f
008000080428     c                   endif
008100080422     c
008200080422     c
008300080428     c                   endif
008400080428     c                   endif
008500080428     c
008600080428     c     kind          reade(N)  cnind00f
008700080422     c                   enddo
008800021216      *
008900021214     C                   eval       *inlr = *on
009000080428     OQSYSPRT   E            ivaerr      1
009100080428     O                       acoksc
009200080428     O                       acorag           +   1
009300080428     o                                        +   1 'Par.IVA errata'
009400080428     O                       indiva           +   1
009500080428     o                                        +   1 'Par.IVA corretta'
009600080428     O                       w007a            +   1
009700080428     OQSYSPRT   E            cdfcanc     1
009800080428     O                       acoksc
009900080428     O                       acorag           +   1
010000080428     o                                        +   1 'Par.IVA'
010100080428     O                       indiva           +   1
010200080428     o                                        +   1 'codFISC da cancellare'
010300080428     O                       indcdf           +   1
