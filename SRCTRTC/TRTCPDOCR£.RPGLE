000100070125     h decedit('0,') option(*nodebugio)
000200070125      *---------------------------------------------------------------*
000300080222      * ricezione esiti da PDA controllo spedizione                   *
000500070125      *---------------------------------------------------------------*
000503080222     ffnarb01l  if   e           k disk
001800080222     d* input rif. spedizione
003201080222     d prmdoc          s             16
003202080225     d* input opzionale rif. distinta
003203080225     d prmfgs          s              3
003204080225     d prmnfv          s              6
003205080222     d* output cod. errore
003206080222     d prmerr          s              1
003207080222     d prmmsg          s             30
009200070129
009300070129      *------------------------------------------------------------------------*
009301080222     c     *entry        plist
009302080222     c                   parm                    prmdoc
009303080225     c                   parm                    prmfgs
009304080225     c                   parm                    prmnfv
009306080222     c                   parm                    prmerr
009307080222     c                   parm                    prmmsg
009308080222     c                   clear                   prmerr
009309080222     c                   clear                   prmmsg
016461080222
016462080222      * rif. spedizione
016463080222     c                   monitor
016464080222     c                   eval      arbaas = %dec(%subst(prmdoc:1:4):4:0)
016465080222     c                   eval      arblnp = %dec(%subst(prmdoc:5:3):3:0)
016466080222     c                   eval      arbnrs = %dec(%subst(prmdoc:8:2):2:0)
016467080222     c                   eval      arbnsp = %dec(%subst(prmdoc:10:7):7:0)
016474080222     c                   on-error
016475080407     c                   eval      prmmsg = 'SPEDIZIONE FORMALMENTE ERRATA'
016476080505     c                   eval      prmerr = '1'
016477080222     c                   exsr      sr_fine
016478080222     c                   endmon
016479080506      * controllo se esiste la spedizione
016481080222     c     kfnarb        chain     fnarb01l
016482080222     c                   if        not %found(fnarb01l)
016483080407     c                   eval      prmmsg = 'SPEDIZ.INESISTENTE SU FNARB00F'
016484080505     c                   eval      prmerr = '2'
016485080222     c                   exsr      sr_fine
016486080222     c                   endif
016487080225     c* controllo congruenza spedizione distinta
016488080225     c                   if        prmfgs <> ' ' and prmfgs <> *all'0'
016489080225     c                             and (arbndc <> %dec(prmnfv : 6: 0) or
016490080225     c                             arbifp <> %dec(prmfgs : 3: 0))
016491080506     c                   eval      prmmsg = 'DIST.FNARB00F <> DSTI.RICEVUTO'
016492080505     c                   eval      prmerr = '3'
016493080225     c                   exsr      sr_fine
016494080225     c                   endif
016495080222     c                   seton                                        rt
016496080222
098400070129      *------------------------------------------------------------------------*
098500080222      * fine
098600070129      *------------------------------------------------------------------------*
098700080222     c     sr_fine       begsr
098703080222     c                   seton                                        rt
098704080222     c                   return
098705080222     c                   endsr
098706080222      *------------------------------------------------------------------------*
098707080222      * inizio
098708080222      *------------------------------------------------------------------------*
098709080222     c     *inzsr        begsr
098800080222     c     kfNARB        klist
098900080222     c                   kfld                    ARBAAS
099000080222     c                   kfld                    ARBLNP
099100080222     c                   kfld                    ARBNRS
099101080222     c                   kfld                    ARBNSP
099300080222     c                   endsr
