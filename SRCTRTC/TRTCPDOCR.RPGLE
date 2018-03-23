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
003208080513     d sped            ds
003209080513     d  aas                           4  0
003210080513     d  lnp                           3  0
003211080513     d  nrs                           2  0
003212080513     d  nsp                           7  0
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
009310080513     c*
009311080513     c                   setoff                                       01
009312080513     c                   testn                   prmfgs               01
009313080513     c                   if        not *in01
009314080513     c                   clear                   prmfgs
009315080513     c                   end
009316080513     c                   testn                   prmnfv               01
009317080513     c                   if        not *in01
009318080513     c                   clear                   prmnfv
009319080513     c                   end
016461080222
016462080222      * rif. spedizione
016463080222     c                   monitor
016464080513     c                   movel     prmdoc        sped
016465080513     c                   move      aas           arbaas
016466080513     c                   move      lnp           arblnp
016467080513     c                   move      nrs           arbnrs
016468080513     c                   move      nsp           arbnsp
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
