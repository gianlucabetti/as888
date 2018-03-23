000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011121     F*----------------------------------------------------*
000400011126     F* CONVERTE GAAME00F IN MZAME00F                      *
001000000000     F*----------------------------------------------------*
001500011126     FGAAME00f  ip   E             DISK
001600011126     FMZAME00F  if a E             DISK
001601011128     Fsysprt    o    f  132        printer oflind(*inof)
001602011126     d comodo          s             15  2
001603011126     d com7            s              7
001604011126     d com3            s              3
001605011126     imzame000
001606011126     i              ametip                      ametip$
001607011126     i              amemar                      amemar$
001608011126     i              amedit                      amedit$
001609011126     i              amecau                      amecau$
001610011126     i              amecdc                      amecdc$
013600000000     C*----------------------------------------------------*
013642011126     c                   eval(h)   ameimt = ameimt / 1936,27
013643011126     c                   eval(h)   ameimp = ameimp / 1936,27
013644011126     c                   eval(h)   ameima = ameima / 1936,27
013645011126     c                   eval(h)   ameimf = ameimf / 1936,27
013646011126     c                   eval      comodo = ameimk * 1000000
013647011128     c                   if        comodo > 150000000
013648011128     c                   except    stampa
013649011128     c                   end
013700011126     c                   eval(h)   ameimk = comodo / 1936,27
013701011126     c                   if        amecdc <> 0
013702011126     c                   movel     amecdc        com7
014200011126     c                   eval      com3 = %subst(com7 :3 :3)
014201011126     c                   clear                   amecdc$
014202011126     c                   movel     com3          amecdc$
014203011126     c                   else
014204011126     c                   movel     *blanks       amecdc$
014205011126     c                   end
014206011126     c                   if        amedac > 20500101
014207011126     c                   movel     19            amedac
014208011126     c                   end
014209011126     c                   if        amesac > 20500101
014210011126     c                   movel     19            amesac
014211011126     c                   end
014212011126     c                   if        amedsp > 20500101
014213011126     c                   movel     19            amedsp
014214011126     c                   end
014215011126     c                   if        amedal > 20500101
014216011126     c                   movel     19            amedal
014217011126     c                   end
014218011126     c                   if        amedur > 20500101
014219011126     c                   movel     19            amedur
014220011126     c                   end
014221011126     c                   if        amedtc > 20500101
014222011126     c                   movel     19            amedtc
014223011126     c                   end
014224011126     c                   if        amedrc > 20500101
014225011126     c                   movel     19            amedrc
014226011126     c                   end
014227011126     c                   if        amedfi > 20500101
014228011126     c                   movel     19            amedfi
014229011126     c                   end
014230011126     c                   movel     ametip        ametip$
014231011126     c                   movel     amemar        amemar$
014232011126     c                   movel     amecau        amecau$
014233011126     c                   if        amedit <> 0
014234011126     c                   movel     amedit        amedit$
014235011126     c                   else
014236011126     c                   movel     *blanks       amedit$
014237011126     c                   end
014238011126     c                   movel     *blanks       amefor
014239011126     c                   z-add     0             ameims
014240011126     c                   z-add     0             amedms
014241011128     c                   if        ametga = '00' or ametgn = '00' or
014242011128     c                             ametla = '00'
014243011128     c                   except    stampa1
014244011128     c                   end
014245011207     c                   movel     '99'          ameusi
014500011126     c                   write     mzame000
014600011128     osysprt    e            stampa      1
014700011128     o                       amenrf        z     +0
014800011128     o                                           +1 'Valore Kasco > di £ 150.00'
014900011128     o                                           +0 '0.000'
015000011128     o          e            stampa1     1
015100011128     o                       amenrf        z     +0
015200011128     o                       ametga              +1
015300011128     o                       ametgn              +1
015400011128     o                       ametla              +1
