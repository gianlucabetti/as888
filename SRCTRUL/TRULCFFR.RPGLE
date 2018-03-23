000001071112?     *--------------------------------------------------------------*
000002071112?     *  TRULCFFR - dato un codice fornitore restituisce il codice fornitore
000003071112      *             di fatturazione
000004071112?     *--------------------------------------------------------------*
002107071112     Fansog01l  IF   E           K DISK
002108071112     Fansog08j  IF   E           K DISK
002110071113     fanfrn01l  uf   e           k disk
002111071112     d trulcffds     e ds
002112071113     d savforfat       s                   like(frnforfatt)
003901071113     c* reperisco il soggetto
003902071113     c     kfrn          chain(n)  anfrn01l
003903071112     c                   if        not %found(anfrn01l)
003904071112     c                   eval      ocfferr = '1'
003905071112     c                   seton                                        lr
003906071112     c                   return
003907071112     c                   else
003908071113     c                   eval      savforfat = frnforfatt
003909071113     c* reperisco la partita iva
003910071112     c     frnsogg       chain     ansog01l
003911071112     c                   if        not %found(ansog01l)
003912071112     c                   eval      ocfferr = '1'
003913071112     c                   seton                                        lr
003914071112     c                   return
003915071112     c                   else
003916071113     c* leggo tutte le anagrafiche con stessa partita iva e stessa società
010701071012     c     sogpartiva    setll     ansog08j
010702071012     c                   do        *hival
010703071012     c     sogpartiva    reade     ansog08j
010704071012     c                   if        %eof(ansog08j)
010705071012     c                   leave
010706071012     c                   end
010707071113     c*
010708071113     c                   if        frnsocieta <> icffsoc
010709071113     c                   iter
010710071113     c                   end
010711071113     c*
010712071113     c                   if        frnksc = icffksc
010713071113     c                   iter
010714071113     c                   end
010715071113     c*
010716071112     c                   if        ocffkscfat <> ' ' and
010717071112     c                             ((frnforfatt <> ' ' and ocffkscfat <>
010718071113     c                             frnforfatt ) or (frnforfatt = ' ' and
010719071113     c                             frnksc <> ocffkscfat))
010720071112     c                   eval      ocfferr = '2'
010721071112     c                   clear                   ocffkscfat
010722071112     c                   seton                                        lr
010723071112     c                   return
010724071112     c                   end
010725071112     c                   if        frnforfatt <> ' '
010726071112     c                   eval      ocffkscfat = frnforfatt
010727071112     c                   else
010728071112     c                   eval      ocffkscfat = frnksc
010729071112     c                   end
011600020404     C                   ENDDO
011601071112     c                   end
011602071112     c                   end
011603071113     c* controllo che se già presente in anagrafica proj un codice fornitore
011604071113     c* sia uguale a quello ricavato dalle altre anagrafiche
011605071113     c                   if        ocffkscfat <> ' ' and savforfat <>
011606071113     c                             ocffkscfat and savforfat <> ' '
011609071113     c                   eval      ocfferr = '2'
011610071113     c                   clear                   ocffkscfat
011611071113     c                   seton                                        lr
011612071113     c                   return
011613071113     c                   end
011614071113     c* aggiorno anagrafico proj per il codice se richiesto
011615071113     c                   if        icfftpa = 'A' and ocfferr= ' ' and
011616071113     c                             ocffkscfat <> ' '
011617071113     c     kfrn          chain     anfrn01l
011618071113     c                   if        %found(anfrn01l)
011619071113     c                   eval      frnforfatt = ocffkscfat
011620071113     c                   update    anfrn000
011621071113     c                   end
011622071113     c                   end
012200951009     C*
012201071012     c                   seton                                        lr
021102071112?     *--------------------------------------------------------------*
021103071112?     *  *INZSR: Operazioni di inizializzazione dati                 *
021104071112?     *--------------------------------------------------------------*
021105071112     C     *INZSR        BEGSR
021200951011      *
021300951011     C     *ENTRY        PLIST
021400071112     C                   PARM                    trulcffds
021401071112     c                   clear                   ocffkscfat
021402071112     c                   clear                   ocfferr
021403071012     c     kfrn          klist
021404071112     c                   kfld                    icffsoc
021405071112     c                   kfld                    icffksc
022500951012     C                   ENDSR
