000100070125     h decedit('0,') option(*nodebugio)
000200070125      *---------------------------------------------------------------*
000300080222      * ricezione esiti da PDA controllo distinta                     *
000500070125      *---------------------------------------------------------------*
000503080222     ffidst01l  if   e           k disk
001800080222     d* input rif. distinta
003201080222     d prmfgs          s              3
003202080222     d prmnfv          s              6
003203110531     d prmdfv          s              8
003204080225     d* input opzionale autotrasp.
003205080225     d prmaut          s              7
003206080222     d* output cod. errore
003207080222     d prmerr          s              1
003208080222     d prmmsg          s             30
003209080513     d fgsds           ds
003210080513     d comfgs                         3  0
003211080513     d nfvds           ds
003212080513     d comnfv                         6  0
009200080513
009300070129      *------------------------------------------------------------------------*
009301080222     c     *entry        plist
009302080222     c                   parm                    prmfgs
009303080222     c                   parm                    prmnfv
009304080225     c                   parm                    prmaut
009305110531     c                   parm                    prmdfv
009306080222     c                   parm                    prmerr
009307080222     c                   parm                    prmmsg
009308080222     c                   clear                   prmerr
009309080222     c                   clear                   prmmsg
009310080513     c*
009311080513     c                   setoff                                       01
009312080513     c                   testn                   prmaut               01
009313080513     c                   if        not *in01
009314080513     c                   clear                   prmaut
009315080513     c                   end
016464080222      * p.o. gestione distinta
016465080513     c                   testn                   prmfgs               01
016466080513     c                   if        *in01
016467080513     c                   move      prmfgs        dstfgs
016468080513     c                   end
016469080222      * numero distinta
016470080513     c                   testn                   prmnfv               01
016471080513     c                   if        *in01
016472080513     c                   move      prmnfv        dstnfv
016473080513     c                   end
016475080513     c                   if        not *in01
016476080407     c                   eval      prmmsg = 'DISTINA FORMALMENTE ERRATA'
016477080506     c                   eval      prmerr = '1'
016478080222     c                   exsr      sr_fine
016479080513     c                   end
016480080222      * controllo se esiste la distinta
016481080222     c     kfidst        chain     fidst01l
016482080222     c                   if        not %found(fidst01l) or dstatb <> *blanks
016483080407     c                   eval      prmmsg = 'DISTINA INESISTENTE FIDST00F'
016484080506     c                   eval      prmerr = '2'
016485080222     c                   exsr      sr_fine
016486080222     c                   endif
016487080225      * se passato AUT controllo se congruente
016489080225     c                   if        prmaut <> ' ' and prmaut <> *all'0'
016490080225     c                             and %dec(prmaut : 7: 0) <> dstpdr
016491080506     c                   eval      prmmsg = 'AUT. FIDST00F <> AUT. RICEVUTO'
016492080506     c                   eval      prmerr = '3'
016493080225     c                   exsr      sr_fine
016494080225     c                   endif
016495110607     c                   move      dstdfv        prmdfv
016496080222     c                   seton                                        rt
016497080222
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
098800080222     c     kfidst        klist
098900080222     c                   kfld                    dstnpg
099000080222     c                   kfld                    dstnfv
099100080222     c                   kfld                    dstfgs
099200080222     c                   eval      dstnpg=4
099300080222     c                   endsr
