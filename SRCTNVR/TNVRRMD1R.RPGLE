000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300110628     Ffiar403l  Uf   E             DISK
000400110628     FWfdpdrmf  o    E             DISK
000403110628     D* DS PER TRUL33R - REPERIMENTO/AGGIORNAMENTO NUMERATORI SU AZNUM
000404110628     D TRUL33DS      E DS
000405110628     d ktrc            s                   like(Ar4trc) inz('I')
000406110628     d kn30            s                   like(Ar4n30) inz('0847')
000407110628     c     kar4          klist
000408110628     c                   kfld                    ktrc
000409110628     c                   kfld                    kn30
000410110628     c     kar4          setll     fiar403l
001000051125     c                   do        *hival
001100110628     c     'I'           reade     fiar403l
001200110628     c                   if        %eof(fiar403l)
001300051125     c                   leave
001400051125     c                   endif
001401110628     c* stacco numero parcel dpd da numeratore automatico
001402110628     c                   clear                   trul33ds
001403110628     c                   z-add     25            i33cnu
001404110628     c                   z-add     1             i33num
001405110628     c                   movel     trul33ds      kpjbu
001406110628     c                   call      'TRUL33R'
001407110628     c                   PARM                    kpjba
001408110628     c                   movel     kpjbu         trul33ds
001409110628     c* se errore numeratore non aggiorno la bolla
001410110628    4c     o33err        ifgt      *zeros
001411110628     c                   else
001412110628     c                   movel     o33nri        §b4ipn
001413110628     c                   clear                   trul28ds4
001414110628     c                   eval      i284cod=§b4ipn
001415110628     c                   call      'TRUL28R5'
001416110628     c                   parm                    trul28ds4
001417110628     c                   movel     o284ckd       §b4icd
001418110628     c                   endif
002400051125     c                   enddo
002500051125     c                   seton                                        lr
