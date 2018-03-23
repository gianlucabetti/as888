000100060105     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300031103     Ftrmz71d   CF   E             WORKSTN
000400030630     Faitra02l  if   e           k disk
000500120329     Faitra04l  if   e           k disk    rename(aitra000:aitra4)
000600031103     Fazorg01l  if   e           k disk
000700060920     Fazorg02l  if   e           k disk    rename(azorg:azorg2)
000800060920     ftabel00f  if   e           k disk
000900031028     Fprtf198   O    F  198        PRINTER prtctl(prtds)
001000020304     dkpjba          e ds
001100030630     d Tibs36Ds      e ds
001200020404     d prtds           ds
001300020404     d   spab                         3  0
001400020404     d   spaa                         3  0
001500020404     d   skab                         3  0
001600020404     d   skaa                         3  0
001700020404     d   line                         3  0
001800020305     d dataiso         s               d   datfmt(*iso)
001900020305     d dataeur         s               d   datfmt(*eur)
002000030903     d wdateur         s               d   datfmt(*eur)
002100030630     d wdatiso         s               d   datfmt(*iso)
002200030903     d tradtms         s              8  0
002300030903     d tradt2s         s              8  0
002400031104     d totale          s              5  0
002500031104     d totfil          s              5  0
002600031104     d exfil           s                   like(trafil)
002700120329     d extrataa        s                   like(trataa)
002800120412     d extraaut        s                   like(traaut)
002900070523     D err             S             78    DIM(9) CTDATA PERRCD(1)
003000070523     D WLBDA8          DS
003100070523     D  G02DAT                 1      8  0
003200070523     D  G02INV                 9     16  0
003300070523     D  G02ERR                17     17
003400070523     D  G02TGI                18     22  0
003500000000     C*---------------------------------------------------------------*
003600020304      *
003700031103     c                   do        *hival
003800031103     c                   exfmt     video1
003900031105     c                   clear                   $msg
004000031104     c                   setoff                                       999796
004100060927     c                   setoff                                       959392
004200031103     c   kc              leave
004300031103     c                   exsr      controv1
004400031104     c   96              iter
004500060920     c                   if        *inkf
004600060920     c                   if        v1cfil <> *zeros  or
004700060920     c                             v1cfil = *zeros and o36pou = 046 and
004800060920     c                             v1care = *blank and v1cdis = *blank
004900060920     c                   exsr      aitra
005000060920     c                   else
005100060920     c                   exsr      areadis
005200060920     c                   endif
005300061019     c                   feod      prtf198
005400061019     c                   z-add     99            line
005500060920     c                   endif
005600060920
005700031103     c                   enddo
005800060921     c                   if        una = *blank and v1cfil <> *zeros
005900031104     c                   except    testata
006000031104     c                   except    noncene
006100031104     c                   endif
006200020308     c                   seton                                        lr
006300990113     c*-------------------------------------------------
006400030630     c     aitra         begsr
006500990113     c*-------------------------------------------------
006600020404     c                   clear                   una               1
006700031103     c                   if        v1cfil = *zeros
006800031103     c     *loval        setll     aitra02l
006900031103     c                   else
007000031103     c     v1cfil        setll     aitra02l
007100031103     c                   endif
007200981029     c                   do        *hival
007300031103     c                   if        v1cfil = *zeros
007400031103     c                   read      aitra02l                               98
007500031103     c                   else
007600031103     c     v1cfil        reade     aitra02l                               98
007700031103     c                   endif
007800031103     c   98              leave
007900061019     c                   if        exfil <> *zeros and trafil <> exfil
008000061019     c                             and una <> *blank
008100061020     c                   exsr      decodfil
008200031104     c                   except    filiale
008300031104     c                   clear                   totfil
008400031104     c                   clear                   una               1
008500031104     c                   endif
008600031104     c                   z-add     trafil        exfil
008700060105      *estrae tutte le anagrafiche con colore mezzo non impostato fuori dai  90 giorni
008800060920     c                   setoff                                       94
008900031003     c                   if        tratmp <> 'S' and tracor = *blank
009000031013     c                             and traris = ' ' and traann = *blank
009100030630     c                   if        tradin > 0
009200030630     c                   move      tradin        dataiso
009300030630     c     wdatiso       subdur    dataiso       diffe:*d          3 0
009400030630     c                   else
009500060105     c                   z-add     90            diffe
009600060920     c                   seton                                        94
009700030630     c                   end
009800120329     c                   if        diffe <  90
009900120427      * salvo date verniciatura prima di verificare accreditamento precedente
010000120427     c                   z-add     tradtm        savdtm            8 0
010100120427     c                   z-add     tradt2        savdt2            8 0
010200120329     c                   exsr      accr_prec
010300120427     c                   z-add     savdtm        tradtm
010400120427     c                   z-add     savdt2        tradt2
010500120427     c                   endif
010600060105     c                   if        diffe >= 90
010700030630     c                   if        line > 60
010800030630     c                   except    testata
010900030630     c                   endif
011000060920     C  n94              move      dataiso       dataeur
011100060920     C   94              clear                   dataeur
011200070730      * verifico se una delle 2 date è nel range di stampa
011300071005     c                   if        v1cda1g > 0
011400070730     c                   if        tradtm >= v1cda1g and tradtm <= v1cda2g
011500070730     c                             or tradt2 >= v1cda1g and tradt2 <= v1cda2g
011600070730     C                   clear                   tradtms
011700070730     C                   clear                   tradt2s
011800070730     c                   select
011900070730      * se la seconda è impostata deve essere nel range per essere stampata
012000070730     c                   when      tradt2 > 0
012100070730     c                   if        tradt2 >= v1cda1g and tradt2 <= v1cda2g
012200070730     c     *ymd          move      tradt2        wdateur
012300070730     c                   move      wdateur       tradt2s
012400071206     c                   if        tradtm > 0
012500071206     c     *ymd          move      tradtm        wdateur
012600071206     c                   move      wdateur       tradtms
012700071206     c                   endif
012800070730     c                   add       1             totfil
012900070730     c                   add       1             totale
013000070730     c                   move      'X'           una
013100070730     c                   except    detta
013200070730     c                   endif
013300070730      * se la prima è impostata deve essere nel range per essere stampata
013400070730      * solo se la seconda non è valorizzata
013500070730     c                   when      tradtm > 0
013600070730     c                   if        tradtm >= v1cda1g and tradtm <= v1cda2g
013700030903     c     *ymd          move      tradtm        wdateur
013800030903     c                   move      wdateur       tradtms
013900071206     c                   if        tradt2 > 0
014000071206     c     *ymd          move      tradt2        wdateur
014100071206     c                   move      wdateur       tradt2s
014200071206     c                   endif
014300070730     c                   add       1             totfil
014400070730     c                   add       1             totale
014500070730     c                   move      'X'           una
014600070730     c                   except    detta
014700030630     c                   endif
014800070730     c                   endsl
014900070523     c                   else
015000070523     c                   iter
015100070523     c                   endif
015200071005      * no date range
015300071005     c                   else
015400071005     C                   clear                   tradtms
015500071005     C                   clear                   tradt2s
015600071005     c                   if        tradt2 > 0
015700071005     c     *ymd          move      tradt2        wdateur
015800071005     c                   move      wdateur       tradt2s
015900071005     c                   add       1             totfil
016000071005     c                   add       1             totale
016100071005     c                   move      'X'           una
016200081103     c                   if        tradtm > 0
016300081103     c     *ymd          move      tradtm        wdateur
016400081103     c                   move      wdateur       tradtms
016500081103     c                   endif
016600071005     c                   except    detta
016700071005     c                   else
016800071005     c                   if        tradtm > 0
016900071005     c     *ymd          move      tradtm        wdateur
017000071005     c                   move      wdateur       tradtms
017100071005     c                   add       1             totfil
017200071005     c                   add       1             totale
017300071005     c                   move      'X'           una
017400071005     c                   except    detta
017500081031     c                   else
017600081031     c                   add       1             totfil
017700081031     c                   add       1             totale
017800081031     c                   move      'X'           una
017900081031     c                   except    detta
018000071005     c                   endif
018100071005     c                   endif
018200071005     c                   endif
018300031104
018400030630     c                   endif
018500030522     c                   endif
018600030630     c                   enddo
018700031105     c                   eval      $msg = 'Elenco stampato '
018800031105     c                   if        una <> *blank
018900061020     c                   exsr      decodfil
019000031104     c                   except    filiale
019100031105     c                   endif
019200031104     c                   if        v1cfil = 0
019300031104     c                   except    globale
019400031104     c                   endif
019500060920     c                   if        v1cdis = *blank and v1care = *blank
019600031104     c                   z-add     99            line
019700060922     c                   else
019800060922     c                   clear                   v1cfil
019900060920     c                   endif
020000060920
020100031104     c                   clear                   totfil
020200031104     c                   clear                   totale
020300031104     c                   clear                   exfil
020400020304
020500020304     c                   endsr
020600981029      *
020700120329     c*-------------------------------------------------
020800120329     c     accr_prec     begsr
020900120329     c*-------------------------------------------------
021000120329     c                   move      trataa        extrataa
021100120412     c                   move      traaut        extraaut
021200120329     c     ktra4         setgt     aitra04l
021300120329     c                   do        *hival
021400120329     c     ktra4         readpe    aitra04l
021500120329     c                   if        %eof(aitra04l)
021600120329     c                   leave
021700120329     c                   endif
021800120412     c                   if        traaut = *blank and trataa <> extrataa or
021900120412     c                             traaut = extraaut and trataa <> extrataa
022000120329     c                   iter
022100120329     c                   endif
022200120329     c                   if        tradfi = 0
022300120329     c                   iter
022400120329     c                   endif
022500120329     c                   move      tradin        dataiso
022600120329     c     wdatiso       subdur    dataiso       diffe:*d          3 0
022700120329     c                   leave
022800120329     c                   enddo
022900120329     c                   endsr
023000061020     c*-------------------------------------------------
023100061020     c     decodfil      begsr
023200061020     c*-------------------------------------------------
023300061020     c                   clear                   exfild           20
023400061020     c     exfil         chain     azorg01l
023500061020     c                   if        %found(azorg01l)
023600061020     c                   movel     orgdes        exfild
023700061020     c                   endif
023800061020
023900061020     c                   endsr
024000031103     c*-------------------------------------------------
024100031103     c     controv1      begsr
024200031103     c*-------------------------------------------------
024300031105     c                   clear                   v1dfil
024400060927      * selezione di P.O /Area/Distretto
024500060920     c                   if        v1cfil > 0 and v1care <> *blank or
024600060920     c                             v1cfil > 0 and v1cdis <> *blank or
024700060920     c                             v1cdis > *blank and v1care <> *blank
024800060927     c                   eval      $msg = err(5)
024900060927     c                   seton                                          96
025000060927     c                   goto      endrut
025100060920     c                   endif
025200060927      * profilo di area non può selezionare distretto
025300060927     c                   if        v1cdis <> *blank and
025400060927     c                             %subst(knmus:1:3) ='REA'
025500060927     c                   eval      $msg = err(1)
025600060927     c                   seton                                          96
025700060927     c                   goto      endrut
025800060927     c                   endif
025900060927      * profilo di distretto non congruente con il distretto richiesto
026000060927     c                   if        v1cdis <> *blank and
026100060927     c                             o36dis <> v1cdis and
026200060927     c                             %subst(knmus:1:3) ='RED'
026300060927     c                   eval      $msg = err(2)
026400060927     c                   seton                                          96
026500060927     c                   goto      endrut
026600060927     c                   endif
026700031104     c                   if        v1cfil > 0
026800031103     c     v1cfil        chain     azorg01l
026900031103     c                   if        %found(azorg01l)
027000031103     c                   movel     orgdes        v1dfil
027100060927      * il po non appartiene al distretto di competenza
027200060927     c                   if        *in90 and orgcar <> o36car and
027300060927     c                             %subst(knmus:1:3) ='REA'
027400060927     c                   eval      $msg = err(3)
027500060927     c                   seton                                          96
027600060927     c                   goto      endrut
027700060927     c                   endif
027800060927      * il po non appartiene al distretto di competenza
027900060927     c                   if        *in90 and orgfl3 <> o36dis and
028000060927     c                             %subst(knmus:1:3) ='RED'
028100060927     c                   eval      $msg = err(4)
028200060927     c                   seton                                          96
028300060927     c                   goto      endrut
028400060927     c                   endif
028500060927
028600031103     c                   else
028700031104     c                   seton                                        9796
028800031103     c                   clear                   v1dfil
028900031103     c                   endif
029000031104     c                   endif
029100060921     c                   clear                   v1cared
029200060921     c                   clear                   v1cdisd
029300060920      * x sede parzializza area o distretto
029400060920     C     v1care        ifeq      '?'
029500060920     C                   movel     '05'          kcod
029600060920     C                   clear                   kkey
029700060920     C                   call      'X§TABER'
029800060920     C                   parm                    kkut
029900060920     C                   parm                    kcod
030000060920     C                   parm                    kkey
030100060920     C                   parm                    §des             30
030200060920     c                   movel     §des          v1cared
030300060925     c                   movel     kkey          v1care
030400060920     C*
030500060920     C                   else
030600060920     C*
030700060925     C                   if        v1care <> *blank and v1care <> *zero
030800060925     C                   movel     '05'          kcod
030900060920     C                   movel(p)  v1care        kkey
031000060920     C     ktab          chain     tabel00f
031100060920     c                   if        %found(tabel00f) and tblflg = *blank
031200060920     C                   movel     tbluni        v1cared
031300060920     c                   else
031400060920     c                   movel     *all'?'       v1cared
031500060920     C                   end
031600060920     C                   end
031700060920     C                   end
031800060920      * distretto
031900060920     C     v1cdis        ifeq      '?'
032000060920     C                   movel     '17'          kcod
032100060920     C                   clear                   kkey
032200060920     C                   call      'X§TABER'
032300060920     C                   parm                    kkut
032400060920     C                   parm                    kcod
032500060920     C                   parm                    kkey
032600060920     C                   parm                    §des
032700060920     c                   movel     §des          v1cdisd
032800060920     c                   movel     kkey          v1cdis
032900060920     C*
033000060920     C                   else
033100060920     C*
033200060925     C                   if        v1cdis <> *blank and v1cdis <> *zero
033300060920     C                   movel     '17'          kcod
033400060920     C                   movel(p)  v1cdis        kkey
033500060920     C     ktab          chain     tabel00f
033600060920     c                   if        %found(tabel00f) and tblflg = *blank
033700060920     C                   movel     tbluni        v1cdisd
033800060920     c                   else
033900060920     c                   movel     *all'?'       v1cdisd
034000060920     C                   end
034100060920     C                   end
034200060920     C                   end
034300060927      * L'area non è di competenza al profilo
034400060927     c                   if        *in90 and v1care <> o36cara and
034500060928     c                             v1care <> *blank and
034600060927     c                             %subst(knmus:1:3) ='REA'
034700060927     c                   eval      $msg = err(6)
034800060927     c                   seton                                          96
034900060927     c                   goto      endrut
035000060927     c                   endif
035100060927
035200060927      * L'area non è di competenza al profilo
035300060927     c                   if        *in90 and v1care <> *blank and
035400060927     c                             %subst(knmus:1:3) ='RED'
035500060927     c                   movel     v1care        areak
035600060927     c     korg          chain     azorg02l
035700060927     c                   if        not %found(azorg02l)
035800060927     c                   eval      $msg = err(7)
035900060927     c                   seton                                          96
036000060927     c                   goto      endrut
036100060927     c                   endif
036200060927     c                   endif
036300060927
036400060920
036500060922     c                   if        *in90 and v1cfil = *zeros and
036600060922     c                             v1cdis = *blank and v1care = *blank
036700031104     c                   seton                                        9996
036800031103     c                   endif
036900070523      *verifica delle date
037000070523     c                   if        v1cda1 <> 0
037100070523     C                   MOVE      V1cda1        G02DAT
037200070523     C                   MOVEL     *BLANK        G02ERR
037300070523     C                   CALL      'XSRDA8'
037400070523     C                   PARM                    WLBDA8
037500070523     C     G02ERR        IFEQ      '1'
037600070523     C                   SETON                                          96
037700070523     c                   eval      $msg = err(8)
037800070523     C                   END
037900070523     c                   move      g02dat        v1cda1
038000070523     c                   move      g02inv        v1cda1g           8 0
038100070523     c                   else
038200070523     c                   clear                   v1cda1g
038300070523     c                   endif
038400070523     c                   if        v1cda2 <> 0
038500070523     C                   MOVE      V1cda2        G02DAT
038600070523     C                   MOVEL     *BLANK        G02ERR
038700070523     C                   CALL      'XSRDA8'
038800070523     C                   PARM                    WLBDA8
038900070523     C     G02ERR        IFEQ      '1'
039000070523     C                   SETON                                          96
039100070523     c                   eval      $msg = err(8)
039200070523     C                   END
039300070523     c                   move      g02dat        v1cda2
039400070523     c                   move      g02inv        v1cda2g           8 0
039500070523     c                   else
039600070523     c                   clear                   v1cda2g
039700070523     c                   endif
039800070523     c                   if        v1cda1g > v1cda2g
039900070523     C                   SETON                                          96
040000070523     c                   eval      $msg = err(9)
040100070523     c                   endif
040200060927     c     endrut        endsr
040300060920     c*-------------------------------------------------
040400060920     c     areadis       begsr
040500060920     c*-------------------------------------------------
040600060920     c     *loval        setll     azorg02l
040700060920     c                   do        *hival
040800060920     c                   read      azorg02l
040900060920      * solo i distretti fino al 5°
041000060920     c                   if        %eof(azorg02l) or orgfl3 > '5'
041100060920     c                   leave
041200060920     c                   endif
041300060920      * solo le filiali
041400060920     c                   if        orgfag <>'F' or %subst(orgde8:1:1) = *blank
041500060920     c                   iter
041600060920     c                   endif
041700060920      * selezionato distretto
041800060920     c                   if        v1cdis <> *blank and v1cdis = orgfl3
041900060920     c                   movel     orgfil        v1cfil
042000060920     c                   exsr      aitra
042100060920     c                   endif
042200060920      * selezionata area
042300060920     c                   move      orgcar        orgcara           3
042400060920     c                   if        v1care <> *blank and v1care = orgcara
042500060920     c                   movel     orgfil        v1cfil
042600060920     c                   exsr      aitra
042700060920     c                   endif
042800060920     c                   enddo
042900060920     c                   endsr
043000011026     c*-------------------------------------------------
043100011026     c     *inzsr        begsr
043200011026     c*-------------------------------------------------
043300020304     c     *entry        plist
043400020304     c                   parm                    kpjba
043500060920     c     ktab          klist
043600060920     c                   kfld                    kkut              1 0
043700060920     c                   kfld                    kcod              2
043800060920     c                   kfld                    kkey              8
043900060920     c                   z-add     1             kkut
044000060920     c                   move      '05'          kcod
044100011026
044200120329     c     ktra4         klist
044300120329     c                   kfld                    trafil
044400120329     c                   kfld                    traaut
044500120329
044600060927     c     korg          klist
044700060927     c                   kfld                    o36dis
044800060927     c                   kfld                    areak             3 0
044900060927
045000020218     C                   time                    w0120            14 0
045100020218     C                   move      w0120         wdat              8 0
045200030630     c                   move      wdat          dataeur
045300030630     c                   move      dataeur       dataiso
045400030630     c                   move      dataiso       wdatiso
045500030630
045600020404     c                   z-add     99            line
045700030630      *profilo di filiale estraggo solo quelli relativi al P.O.
045800030630     C                   CLEAR                   Tibs36Ds
045900030630     C                   EVAL      I36ute = knmus
046000030630     C                   CALL      'TIBS36R'
046100030630     C                   PARM                    tibs36ds
046200060927     C                   movel     o36car        o36cara           3
046300030630      *se profilo di filiale accende 90 per pilotare le letture solo sul P.O
046400030630     c                   if        o36pou <> 046
046500030630     c                   seton                                        90
046600030630     c                   endif
046700031103     c                   move      o36pou        v1cfil
046800031103     c     v1cfil        chain     azorg01l
046900031103     c                   if        %found(azorg01l)
047000031103     c                   movel     orgdes        v1dfil
047100031103     c                   endif
047200060920      * verifica se profili di filiale con autorizzazione a area o distretto
047300060920      * oppure se profilo di sede nel caso abilita selezione area e distretto
047400060920     c                   if        %subst(knmus:1:3) ='REA' or
047500060920     c                             %subst(knmus:1:3) ='RED' or
047600060920     c                             o36pou = 046
047700060920     c                   seton                                        89
047800060927     c                   else
047900060927     c                   seton                                        88
048000060920     c                   endif
048100011026     c                   endsr
048200020404     C*--------------------------------------------------------------------
048300031028     Oprtf198   E            testata        1 02
048400030630     o                                           10 'TRMZ71R'
048500020404     o                       knmus               22
048600030630     o                                        +   1 'Lista autisti accreditati -
048700030630     o                                              con mezzo non verniciato'
048800030630     o                       udate         y  +   1
048900020404     O                       page          z  +   1
049000020404     O          E            testata     1  1
049100030630     o                                           20 'Autotrasportatore'
049200030630     o                                           45 'Società'
049300030909     o                                           65 'Targa'
049400030909     o                                           80 'P.O'
049500030909     o                                           92 'Data Accr.'
049600030909     o                                          104 'Prev.Vern.'
049700030909     o                                          116 'Prev.Vern.'
049800031028     o                                          130 'Note'
049900020404     O          E            detta       1
050000030630     o                       traaut              25
050100030630     o                       trasoc              52
050200030909     o                       trataa              67
050300030909     o                       trafil              80
050400030909     o                       dataeur             92
050500030909     o                       tradtms       8    104
050600030909     o                       tradt2s       8    116
050700031028     o                       trant1             190
050800031104     O          E            filiale     1
050900031104     o                                           25 'T o t a l e  p e r  P.O'
051000031104     o                       exfil               30
051100061019     o                       exfild              52
051200061019     o                       totfil        z     62
051300061019     O          E            filiale     0
051400061019     o                                           25 'T o t a l e  p e r  P.O'
051500061019     o                       exfil               30
051600061019     o                       exfild              52
051700061019     o                       totfil        z     62
051800061019     O          E            filiale        1
051900061019     o                                           25 'T o t a l e  p e r  P.O'
052000061019     o                       exfil               30
052100061019     o                       exfild              52
052200061019     o                       totfil        z     62
052300031104     O          E            globale     1
052400031104     o                                           30 'T o t a l e  G e n e r a -
052500031104     o                                              l e'
052600031104     o                       totale        z     40
052700020404     O          E            noncene     3
052800020404     o                                           80 'N O N   E S I S T O N O   -
052900030630     o                                              A U T I S T I   D A  C O N -
053000020404     o                                              V A L I D A R E'
053100060927      *__________________________________________________________________
053200060927** ERR
053300060927Profilo di area non può selezionare distretto                                  1
053400060927Il distretto richiesto è incongruente con il profilo                           2
053500060927Il P.O richiesto non appartiene all'area di competenza                         3
053600060927Il P.O richiesto non appartiene al distretto di competenza                     4
053700060927Selezionare solo uno fra P.O / Area / Distretto                                5
053800070523L'area non è di competenza al profilo                                          6
053900070523L'area selezionata non appartiene al distretto di competenza                   7
054000070523Data non valida                                                                8
054100070523Incongruenza nelle date immesse                                                9
