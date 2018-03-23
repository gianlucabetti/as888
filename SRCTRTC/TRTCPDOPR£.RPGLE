000100070125     h decedit('0,') option(*nodebugio)
000200070125      *---------------------------------------------------------------*
000300080222      * ricezione esiti da PDA controllo spedizione                   *
000500070125      *---------------------------------------------------------------*
001800080222     d* input data ora pda
003201080222     d prmdatora       s             14
003205080222     d* output cod. errore
003206080222     d prmerr          s              1
003207080222     d prmmsg          s             30
003208080222     d data            s               d
003209110606     d datiso          s               d
003210110606     d oggimeno30      s               d   datfmt(*eur)
003211080222     d ora             s               t
003212080513     d ctrdatads       ds
003213080513     d  datac                  1      8  0
003214080513     d  orac                   9     14  0
009200070129
009300070129      *------------------------------------------------------------------------*
009301080222     c     *entry        plist
009302080222     c                   parm                    prmdatora
009306080222     c                   parm                    prmerr
009307080222     c                   parm                    prmmsg
009308080222     c                   clear                   prmerr
009309080222     c                   clear                   prmmsg
009310080513     c                   move      prmdatora     ctrdatads
009311080222     c                   monitor
009312110606     c                   time                    comtime          14 0
009313110606     c                   move      comtime       oggimeno30
009314110531     c                   subdur    30:*d         oggimeno30
009315110606     c                   move      oggimeno30    datiso
009316080513     c                   eval      data= %date(%subst(ctrdatads: 1: 8): *iso0)
009317080513     c                   eval      ora = %time(%subst(ctrdatads: 9: 6): *iso0)
009318080222     c                   on-error
009319110531     c                   eval      prmmsg = 'DATA ORA FORMALMENTE ERRATA'
009320080222     c                   eval      prmerr = 'E'
009321080222     c                   endmon
009322110531     c* se non c'è errore formale verifico che la data non sia antecedente
009323110531     c* a udate - 30 gg
009324110606     c                   if        prmerr = ' ' and data < datiso
009325110601     c                   eval      prmerr = 'F'
009326110606     c*                  eval      prmmsg = 'DATA ORA < DI OGGI - 30'
009327110531     c                   end
016461080222
016487080222     c                   seton                                        rt
016488080222
