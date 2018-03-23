000100050404     H DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000200050404
000300050404      *------------------------------------------------------------------------*
000400050404      *                                                                        *
000500121030      *    Pulisce flag 3 campo CLSFLO Pagamento Danni Consuldanni Vettore     *
000600050404      *                                                                        *
000700050404      *------------------------------------------------------------------------*
000800050404
000900050404     ffncls00f  up   e           k disk
001000050404     fcnaco00f  uf   e           k disk
001100050404
001200050404     d codut           s              1  0 inz(1)
001300050404     d kci             s              4  0 inz(151)
001400050404
001500050404     c     kcnaco        klist
001600050404     c                   kfld                    codut
001700050404     c                   kfld                    kci
001800050404     c                   kfld                    clsksc
001900050404
002000121030     c                   If        %subst(clsflo:3:1) <> *Blanks
002100121030     c                   eval      %subst(clsflo:3:1) = ' '
002200050404      * sflaggo cnaco
002300050404     c     kcnaco        chain     cnaco00f
002400050404     c                   if        %found(cnaco00f)
002500050404     c                   clear                   acoftr
002600050404     c                   clear                   acodtr
002700050404     c                   update    cnaco000
002800050404     c                   endif
002900050404      * aggiorno fncls
003000050404     c                   update    fncls000
003100050404     c                   endif
