000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200110321     FTITAS39c  uF   E           K DISK    ignore(titas000)
000201110314     F                                     ignore(titasp00)
000202110314     FTncsb03L  iF   E           K DISK
000203110314     Ffiar531c  iF   E           K DISK
000204110314     Fazorg01l  iF   E           K DISK
000205110314     FQSYSPRT   O    F  132        PRINTER oflind(*inof)
000400050228
001000110314     d klnp            s                   like(TASlnp)
001001110314     d kaas            s                   like(TASaas)
001100110314     d kmgs            s                   like(TASmgs)
001101110314     d anno            s              4s 0
001102110314     d mese            s              2s 0
001103110314     d gg              s              2s 0
001107110314     d dtasflo       e ds
001108110314     d dar5gen       e ds
001109110314      * - Parametri per TRUL21R (calcolo limiti importo c/assegno)
001110110314     d TRUL21ds      e ds                  inz
001111110314     d  I21tla       e                     inz('L')
001200050228
001500050228      *
001501110314     c     *entry        plist
001503110314     c                   parm                    anno
001504110314     c                   parm                    mese
001505110314     c                   parm                    gg
001509110314      *
001600110314     C     KTASa         KLIST
001700110314     C                   KFLD                    Klnp
001701110314     C                   KFLD                    Kaas
001702110314      *
001703110314     C     KTAS          KLIST
001704110314     C                   KFLD                    Klnp
001705110314     C                   KFLD                    Kaas
001800110314     C                   KFLD                    kmgs
001801110314      *
001802110314     C     Kcsb          KLIST
001803110314     C                   KFLD                    tasaas
001804110314     C                   KFLD                    taslnp
001805110314     C                   KFLD                    tasnrs
001806110314     C                   KFLD                    tasnsp
001807110314      *
001808110314     C     Kar5gen       KLIST
001809110314     C                   KFLD                    tasaas
001810110314     C                   KFLD                    taslnp
001811110314     C                   KFLD                    tasnrs
001812110314     C                   KFLD                    tasnsp
001813110314     C                   KFLD                    ar5trd
001814110314     c                   eval      ar5trd = 'GEN'
001815110314     c*
001816110314     c                   seton                                        lr
001818110314     c                   if        anno = 0
001819110314     c                   return
001820110314     c                   end
002800110314     c                   eval      kaas = anno
002801110314     c     *loval        setll     azorg01l
002802110314     c                   do        *hival
002803110314     c                   read      azorg01l
002804110314     c                   if        %eof(azorg01l)
002805110314     c                   leave
002806110314     c                   end
002807110314     c                   if        ORGFVA <> ' ' or
002808110314     c                             ORGFAG <> 'F'
002809110314     c                   iter
002810110314     c                   end
002811110314     c                   eval      klnp = orgfil
002900050228      *
002902110314     c                   select
002903110314     c                   when      mese = 0
003000110314     C     ktasa         SETLL     TItas39c
003001110314     c                   when      mese <> 0
003002110314     c                   movel     mese          kmgs
003003110314     c                   move      gg            kmgs
003100110314     C     Ktas          setll     TItas39c
003101110314     c                   endsl
003200050228      *
003300110314    1C                   DO        *hival
003301110314     c                   select
003302110314     c                   when      mese = 0  or gg = 0
003303110314     C     ktasa         reade     TItas39c
003304110314     c                   when      mese <> 0 and gg <> 0
003307110314     C     Ktas          reade     TItas39c
003309110314     c                   endsl
003310110314     c                   if        %eof(titas39c)
003311110314     c                   leave
003312110314     c                   end
003313110314     c* tutto un mese
003314110314     c                   if        mese <> 0 and gg = 0
003315110314     c                   movel     tasmgs        mesecom           2 0
003316110314     c                   if        mese <> mesecom
003317110314     c                   leave
003318110314     c                   end
003319110314     c                   end
003320110314     c* flag di c/ass.
003321110314     c                   if        tasfca <> 'S'
003322110314     c                   iter
003323110314     c                   end
003324110314     c     kcsb          chain     tncsb03l
003325110314     c                   if        not %found(tncsb03l) or
003326110314     c                             csbsta = 9
003327110314     c                   iter
003328110314     c                   end
003329110314     c     kar5gen       chain     fiar531c
003330110314     c                   if        not %found(fiar531c)
003332110314     c                   iter
003333110314     c                   end
003334110314     c                   eval      dar5gen = ar5uni
003339110314     c* verifico se devo aggiornare
003340110314     c                   reset                   TRUL21DS
003341110314     c                   eval      I21cbo =  tascbo
003342110314     c                   eval      I21tsp =  tastsp
003343110314     c                   eval      I21lnp =  taslnp
003344110314     c                   eval      I21nzm =  tasnzm
003345110314     c                   eval      I21lna =  taslna
003346110314     c                   eval      I21nzd =  tasnzd
003347110314     c                   eval      I21tic =  §AR5TICI
003348110314     c                   eval      I21imp =  csbcas
003349110314     c                   eval      I21div =  csbvca
003350110314     c                   call      'TRUL21R'
003351110314     c                   parm                    TRUL21ds
003352110314     c                   if        O21fca <> *blanks
003354110314     c                   eval      dtasflo = tasflo
003355110314     c                   eval      §flofca = 'S'
003356110314     c                   eval      tasflo = dtasflo
003357110321     C                   UPDATE    TITAS010
003358110321     C*                  EXCEPT    STAMPA
003359110314     c                   endif
006500980703     C                   ENDDO
006501110314     C                   ENDDO
006600110314     OQSYSPRT   E            stampa      1
006700110314     O                       TASAAS        Z
006800110314     O                       TASLNP        Z   +  1
006900110314     O                       TASNRS        Z   +  1
007000110314     O                       TASNSP        Z   +  1
007100110314     O                       TASLNA        Z   +  1
007200110314     O                       TASMGS            +  1 '  /  '
007300110314     O                       §ar5tici          +  1
007301110314     O                       csbvca            +  1
007400110314     O                       csbcas        4   +  1
007600110314     O                       o21fca            +  1
