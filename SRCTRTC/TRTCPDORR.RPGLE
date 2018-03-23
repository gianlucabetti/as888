000100070125     h decedit('0,') option(*nodebugio)
000200070125      *---------------------------------------------------------------*
000300080222      * ricezione esiti da PDA controllo spedizione                   *
000500070125      *---------------------------------------------------------------*
001800080222     d* input data ora pda
003201131128     d prmora4         s              4
003202131128     d prmordin        s              4
003205080222     d* output cod. errore
003206080222     d prmerr          s              1
003207080222     d prmmsg          s             30
003215131128     d ora4            s              4  0
003216131128     d ordine          s              4  0
009200070129
009300070129      *------------------------------------------------------------------------*
009301080222     c     *entry        plist
009302131128     c                   parm                    prmora4
009303131128     c                   parm                    prmordin
009306080222     c                   parm                    prmerr
009307080222     c                   parm                    prmmsg
009308080222     c                   clear                   prmerr
009309080222     c                   clear                   prmmsg
009310131128      * controllo ora
009312131128     c                   move      prmora4       ora4
009313080222     c                   monitor
009321080222     c                   on-error
009322131128     c                   eval      prmmsg = 'ORA FORMALMENTE ERRATA'
009323080222     c                   eval      prmerr = 'E'
009324080222     c                   endmon
009325131128      * controllo ordinamento
009326131128     c                   monitor
009328131128     c                   move      prmordin      ordine
009332131128     c                   on-error
009333131128     c                   eval      prmmsg = 'ORDINAMENTO ERRATO'
009334131128     c                   eval      prmerr = '1'
009335131128     c                   endmon
016468120924
016487080222     c                   seton                                        rt
016488080222
