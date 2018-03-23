000100050404     H DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000200050404
000300050404      *------------------------------------------------------------------------*
000400050404      *                                                                        *
000500121030      *    Pulisce flag 3 campo CLSFLO Pagamento Danni Consuldanni Vettore     *
000600050404      *                                                                        *
000700050404      *------------------------------------------------------------------------*
000800050404
000900121030     ftfcls00f  up   e           k disk
001000050404
001100050404     d codut           s              1  0 inz(1)
001200050404     d kci             s              4  0 inz(151)
001300050404
001400050404
001500121030     c                   If        %subst(clsflo:3:1) <> *Blanks
001600121030     c                   eval      %subst(clsflo:3:1) = ' '
001700050404      * aggiorno fncls
001800050404     c                   update    fncls000
001900050404     c                   endif
