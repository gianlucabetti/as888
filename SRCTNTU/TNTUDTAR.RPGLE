000400051229     ftntbe01l  uf   e           k disk
000602051229     dtntudtads      e ds
000603051229     ddtup           e ds                  inz
002000030115
005100051129     C     *ENTRY        PLIST
005200051213     C                   PARM                    tntudtads
005201051229     c     ktbe          klist
005202051229     c                   kfld                    tbecod
005203051229     c                   kfld                    tbeke1
005204051229     c                   eval      tbecod = 'TUP'
005205051229     c                   eval      tbeke1 = §DTAazn
005206051229     c     ktbe          chain     tntbe01l
005207051229     c                   if        %found(tntbe01l)
005208051229     c                   movel     tbeuni        dtup
005209060112     c                   if        §dtadta > §tupdta
005210051229     c                   eval      §TUPDTA = §DTADTA
005211051229     c                   eval      tbeuni = dtup
005212051229     c                   update    tntbe000
005213051229     c                   end
005214060112     c                   end
005215051229     c                   seton                                        lr
007300051129     c*
