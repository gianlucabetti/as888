000100981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300081204     Ftrmz73d   CF   E             wORKSTN
000400030630     Faitra02l  if   e           k disk
000500090625     Faitrs01l  if   e           k disk
000600050623     Fazorg01l  if   e           k disk    rename(azorg:azorg1)
000700050622     Fazorg02l  if   e           k disk
000800050802     Fwaitra0f  uf a e           k disk    usropn
000900050801     ftabel00f  if   e           k disk
001000050805     Fqsysprt   O    F  132        PRINTER prtctl(prtds)
001100020304     dkpjba          e ds
001200030630     d Tibs36Ds      e ds
001300081204     D tibs02ds      E DS
001400081204
001500020404     d prtds           ds
001600020404     d   spab                         3  0
001700020404     d   spaa                         3  0
001800020404     d   skab                         3  0
001900020404     d   skaa                         3  0
002000020404     d   line                         3  0
002100050623     d totmzfil        s              5  0
002200050623     d totokfil        s              5  0
002300050623     d totoldfil       s              5  0
002400050623     d totmzarea       s              5  0
002500050623     d totokarea       s              5  0
002600050623     d totoldarea      s              5  0
002700050623     d totmzdis        s              5  0
002800050623     d totokdis        s              5  0
002900050623     d totolddis       s              5  0
003000050623     d totmzaz         s              5  0
003100050623     d totokaz         s              5  0
003200050623     d totoldaz        s              5  0
003300050623     d limite          s              8  0
003400051121     d anno2           s              2  0
003500050623     d primo           s              1
003600050801     d exaread         s             14
003700051121     d exaread20       s             20
003800051121     d exdistd20       s             20
003900050623     d excar           s                   like(orgcar)
004000050802     d warea           s                   like(orgcar)
004100050623     d kfil            s                   like(orgfil)
004200050803     d exfil           s                   like(orgfil)
004300050623     d exfl3           s                   like(orgfl3)
004400050802     d wdist           s                   like(orgfl3)
004500050802     d coman           s             66
004600050802     d luncmd          s             15  5
004700050802     D cmd1            S              1    DIM(66) CTDATA PERRCD(66)
004800050802     D cmd2            S              1    DIM(66) CTDATA PERRCD(66)
004900050802     D cmd3            S              1    DIM(66) CTDATA PERRCD(66)
005000000000     C*---------------------------------------------------------------*
005100020304      *
005200031105     c                   do        *hival
005300031105     c                   exfmt     video1
005400031105     c                   clear                   $msg
005500031105     c   kc              leave
005600031105     c                   exsr      controv1
005700031105     c   96              iter
005800050623     c                   if        *inkf
005900050805      * verifico indicatore per salto pagina forzato
006000050805     c     saltp         comp      'S'                                    66
006100051017     c     vdet          comp      'S'                                    90
006200050802      *clear file
006300050802     c                   movea     cmd1          coman
006400050802     c                   exsr      comandi
006500050802
006600050623     c                   movel     v1cdal        limite
006700050623     c                   move      '0101'        limite
006800051121     c                   move      v1cdal        anno2
006900050802     c  n90              except    testata
007000050623     c                   exsr      elabora
007100050805     c                   leave
007200050623     c                   endif
007300050805     c                   enddo
007400050802      * se richiesta stampa per area o distretto la eseguo dopo aver
007500050802      * completato il W.F con i dati selezionati
007600050802     c                   if        lisarea = 'S' and *in80 or
007700050802     c                             lisdist = 'S' and *in81 or *in90
007800050802     c                   exsr      waitra
007900050802     c                   endif
008000050802      *elimina file in qtemp x la filiale
008100051017     c                   movea     cmd3          coman
008200051017     c                   exsr      comandi
008300050802
008400020308     c                   seton                                        lr
008500050622     c*-------------------------------------------------
008600050622     c     elabora       begsr
008700050622     c*-------------------------------------------------
008800050622     c                   clear                   v1dfil
008900051122     c                   if        v1cfil > 0 and lisarea = *blank and
009000050802     c                             lisdist = *blank
009100050623     c                   move      v1cfil        kfil
009200050622     c                   exsr      aitra
009300050622     c                   else
009400050622     c     *loval        setll     azorg02l
009500050622     c                   do        *hival
009600050622     c                   read      azorg02l
009700050623     c                   if        %eof(azorg02l) or orgfl3 > '5'
009800050622     c                   leave
009900050622     c                   endif
010000051122      * SEDE solo le filiali di un'area se selezionata a video
010100051122     c                   move      orgcar        orgcara           3
010200051122     c                   if        orgcara <> varea and varea <> *blank
010300051122     c                   iter
010400051122     c                   endif
010500051122      * SEDE solo le filiali di un distretto se selezionato a video
010600051122     c                   if        orgfl3 <> vdist and vdist <> *blank
010700051122     c                   iter
010800051122     c                   endif
010900050623      * solo le filiali
011000050805     c                   if        orgfag <>'F' or %subst(orgde8:1:1) = *blank
011100050623     c                   iter
011200050623     c                   endif
011300050802      * scarto se profilo di area o di distretto e voglio sapere solo i miei
011400050802     c                   if        lisarea = 'S' and *in80
011500050802     c                   if        orgcar <> warea
011600050802     c                   iter
011700050802     c                   endif
011800050802     c                   endif
011900050802     c                   if        lisdist = 'S' and *in81
012000050802     c                   if        orgfl3 <> wdist
012100050802     c                   iter
012200050802     c                   endif
012300050802     c                   endif
012400050622      * rottura di area stampo il totale passo a nuova pagina con nuova area
012500050623     c                   if        excar <> orgcar and primo <> *blank
012600050801     c                   if        line > 60
012700050802     c  n90              except    testata
012800050801     c                   endif
012900050801      * descrizione area
013000050801     c                   clear                   exaread
013100050801     c                   movel     excar         kkey
013200051121     c                   move      '05'          kcod
013300050801     c     ktab          chain     tabel00f
013400050801     c                   if        %found(tabel00f)
013500050801     c                   movel     tbluni        exaread
013600050801     c                   endif
013700050802     c  n90              except    area
013800050805     c   66              z-add     99            line
013900050623     c                   clear                   totmzarea
014000050623     c                   clear                   totokarea
014100050623     c                   clear                   totoldarea
014200050622     c                   move      orgcar        excar
014300050622     c                   endif
014400050623      * rottura di distretto
014500050623     c                   if        exfl3 <> orgfl3 and primo <> *blank
014600050801     c                   if        line > 60
014700050802     c  n90              except    testata
014800050801     c                   endif
014900050802     c  n90              except    dist
015000050805     c   66              z-add     99            line
015100050623     c                   clear                   totmzdis
015200050623     c                   clear                   totokdis
015300050623     c                   clear                   totolddis
015400050623     c                   move      orgfl3        exfl3
015500050623     c                   endif
015600050623     c                   if        primo = *blank
015700050623     c                   move      orgcar        excar
015800050623     c                   move      orgfl3        exfl3
015900050623     c                   move      'X'           primo
016000050623     c                   endif
016100050622      * conteggia automezzi per P.O
016200050623     c                   move      orgfil        kfil
016300050622     c                   exsr      aitra
016400050622     c                   enddo
016500050623      * stampa fine azorg
016600050801     c                   if        line > 60
016700050802     c  n90              except    testata
016800050801     c                   endif
016900050801      * descrizione area
017000050801     c                   clear                   exaread
017100050801     c                   movel     excar         kkey
017200051121     c                   move      '05'          kcod
017300050801     c     ktab          chain     tabel00f
017400050801     c                   if        %found(tabel00f)
017500050801     c                   movel     tbluni        exaread
017600050801     c                   endif
017700050802     c  n90              except    area
017800050805     c   66              z-add     99            line
017900050801     c                   if        line > 60
018000050802     c  n90              except    testata
018100050801     c                   endif
018200050802     c  n90              except    dist
018300050805     c   66              z-add     99            line
018400050801     c                   if        line > 60
018500050802     c  n90              except    testata
018600050801     c                   endif
018700050802     c  n90              except    azienda
018800050623     c                   endif
018900050622
019000050622     c                   endsr
019100990113     c*-------------------------------------------------
019200030630     c     aitra         begsr
019300990113     c*-------------------------------------------------
019400050623     c     kfil          setll     aitra02l
019500981029     c                   do        *hival
019600050623     c     kfil          reade     aitra02l                               98
019700031105     c   98              leave
019800090625     c     tranrc        chain     aitrs01l
019900090625     c                   if        %found(aitrs01l)
020000090625     c                   movel     trsiva        traivas
020100090625     c                   else
020200090625     c                   clear                   traivas
020300090625     c                   endif
020400090625      *
020500050623     c                   if        traann <> *blank or traris <> *blank or
020600050623     c                             tracor <> *blank
020700040212     c                   iter
020800040212     c                   endif
020900050623      * incremento totale mezzi per area e filiale
021000050623     c                   add       1             totmzfil
021100050623     c                   add       1             totmzarea
021200050623     c                   add       1             totmzdis
021300050623     c                   add       1             totmzaz
021400050623
021500050623      * incremento totale mezzi sopra o sotto il limite per area e filiale
021600050623     c                   if        tradass >= limite
021700050623     c                   add       1             totokfil
021800050623     c                   add       1             totokarea
021900050623     c                   add       1             totokdis
022000050623     c                   add       1             totokaz
022100050802      * se richiesta  per area o distretto non scrivo i mezzi con data sopra
022200050802      * il limite nel W.F
022300050802     c                   if        lisarea = 'S' and *in80 or
022400051017     c                             lisdist = 'S' and *in81 or *in90
022500050802     c                   iter
022600050802     c                   endif
022700050802
022800050623     c                   else
022900050623     c                   add       1             totoldfil
023000050623     c                   add       1             totoldarea
023100050623     c                   add       1             totolddis
023200050623     c                   add       1             totoldaz
023300050623     c                   endif
023400050801      * scrive W.F
023500050801     c                   move      orgcar        traarea
023600050801     c                   move      orgfl3        tradist
023700050801     c                   movel     tradass       traanno
023800050801     c                   write     waitra00
023900050623     c                   enddo
024000050623
024100050623     c                   if        line > 60
024200050802     c  n90              except    testata
024300050623     c                   endif
024400050802     c  n90              except    filiale
024500050623     c                   clear                   totmzfil
024600050623     c                   clear                   totokfil
024700050623     c                   clear                   totoldfil
024800020304
024900020304     c                   endsr
025000981029      *
025100050802     c*-------------------------------------------------
025200050802     c     waitra        begsr
025300050802     c*-------------------------------------------------
025400050803     c                   clear                   totmzfil
025500050803     c                   clear                   totmzarea
025600050803     c                   clear                   totmzdis
025700051018     c                   clear                   totmzaz
025800050803     c                   clear                   primo
025900050802     c     *loval        setll     waitra0f
026000050802     c                   do        *hival
026100050802     c                   read      waitra0f                               98
026200050802     c   98              leave
026300051121      * filtro area (solo x sede)
026400051121     c                   if        varea <> *blank
026500051121     c                   move      traarea       traareaa          3
026600051121     c                   if        traareaa <> varea
026700051121     c                   iter
026800051121     c                   endif
026900051121     c                   endif
027000051121      * filtro distretto (solo x sede)
027100051121     c                   if        vdist <> *blank and tradist <> vdist
027200051121     c                   iter
027300051121     c                   endif
027400051121      *
027500050803     c                   if        primo = *blank
027600050803     c                   move      trafil        exfil
027700050803     c                   move      traarea       excar
027800050803     c                   move      tradist       exfl3
027900050803     c                   move      'X'           primo
028000050803     c                   endif
028100050803      * rottura filiale
028200050803     c                   if        trafil  <> exfil
028300050803     c                   if        line > 60
028400051017     c   90              except    testafil
028500050803     c                   endif
028600051121     c     exfil         chain     azorg01l
028700051017     c   90              except    totfil
028800050803     c                   clear                   totmzfil
028900050803     c                   move      trafil        exfil
029000050803     c                   endif
029100050803      *
029200050803      * rottura area
029300050803     c                   if        traarea <> excar
029400050803     c                   if        line > 60
029500051017     c   90              except    testafil
029600050803     c                   endif
029700050803      * descrizione area
029800051121     c                   clear                   exaread20
029900050803     c                   movel     excar         kkey
030000051121     c                   move      '05'          kcod
030100050803     c     ktab          chain     tabel00f
030200050803     c                   if        %found(tabel00f)
030300051121     c                   movel     tbluni        exaread20
030400050803     c                   endif
030500051017     c   90              except    detarea
030600050803     c                   clear                   totmzarea
030700050803     c                   move      traarea       excar
030800050805     c   66              z-add     99            line
030900050803     c                   endif
031000050803      *
031100051018      * rottura distretto
031200051018     c                   if        tradist <> exfl3
031300051018     c                   if        line > 60
031400051018     c   90              except    testafil
031500051018     c                   endif
031600051121     c                   clear                   exdistd20
031700051121     c                   movel(p)  exfl3         kkey
031800051121     c                   move      '17'          kcod
031900051121     c     ktab          chain     tabel00f
032000051121     c                   if        %found(tabel00f)
032100051121     c                   movel     tbluni        exdistd20
032200051121     c                   endif
032300051018     c   90              except    detdist
032400051018     c                   clear                   totmzdis
032500051018     c                   move      tradist       exfl3
032600051018     c   66              z-add     99            line
032700051018     c                   endif
032800051018      * dettaglio  p.o
032900050802     c                   if        line > 60
033000051017     c   90              except    testafil
033100050802     c                   endif
033200051121     c     exfil         chain     azorg01l
033300081204      *decodifica allestimento
033400081204     c                   clear                   trafurd          30
033500081204     c                   if        trafur <> *blank
033600081204     c                   clear                   tibs02ds
033700081204     c                   movel     'C'           t02mod
033800081204     c                   movel     knsif         t02sif
033900081204     c                   movel     'ALL'         t02cod
034000081204     c                   movel     trafur        t02ke1
034100081204     c                   call      'TIBS02R'
034200081204     c                   parm                    KPJBA
034300081204     c                   parm                    TIBS02DS
034400081204     c                   if        t02err =  *blanks
034500081204     c                   movel     t02uni        trafurd
034600081204     c                   endif
034700081204     c                   endif
034800051017     c   90              except    detfil
034900050802      * incremento totale mezzi per area e filiale
035000050802     c                   add       1             totmzfil
035100050802     c                   add       1             totmzarea
035200050802     c                   add       1             totmzdis
035300051018     c                   add       1             totmzaz
035400050802
035500050802     c                   enddo
035600051018
035700051018      * fine file controlli finali x stampa totali
035800050803     c                   if        lisarea = 'S' and *in80 or
035900051018     c                             lisdist = 'S' and *in81 or *in90
036000050803     c                   if        line > 60
036100051017     c   90              except    testafil
036200050803     c                   endif
036300050803      * descrizione area
036400051121     c                   clear                   exaread20
036500050803     c                   movel     excar         kkey
036600051121     c                   move      '05'          kcod
036700050803     c     ktab          chain     tabel00f
036800050803     c                   if        %found(tabel00f)
036900051121     c                   movel     tbluni        exaread20
037000050803     c                   endif
037100051122     c   90              except    totfil
037200051018      * stampo totale area finale
037300051018     c                   if        *in80 or *in81 or *in82 and v1cfil = 0
037400051122     c   90              except    detarea
037500051018     c                   endif
037600051018      * stampo totale distretto finale
037700051018     c                   if        *in81 or *in82 and v1cfil = 0
037800051121     c                   clear                   exdistd20
037900051121     c                   movel(p)  exfl3         kkey
038000051121     c                   move      '17'          kcod
038100051121     c     ktab          chain     tabel00f
038200051121     c                   if        %found(tabel00f)
038300051121     c                   movel     tbluni        exdistd20
038400051121     c                   endif
038500051122     c   90              except    detdist
038600051018     c                   endif
038700051018      * stampo totale aziendale solo se richiesta globale di sede
038800051018     c                   if        *in82 and v1cfil = 0
038900051018     c                   except    detaz
039000050803     c                   endif
039100050803
039200051018     c                   endif
039300050802     c                   endsr
039400050802      *
039500031105     c*-------------------------------------------------
039600031105     c     controv1      begsr
039700031105     c*-------------------------------------------------
039800031105     c                   clear                   v1dfil
039900050803     c                   setoff                                       969799
040000051122     c                   setoff                                       959493
040100051122     c                   setoff                                       92
040200031105     c                   if        v1cfil > 0
040300051122
040400050623     c     v1cfil        chain     azorg01l
040500050623     c                   if        %found(azorg01l)
040600031105     c                   movel     orgdes        v1dfil
040700031105     c                   else
040800031105     c                   seton                                        9796
040900051122     c                   goto      endcontr
041000031105     c                   endif
041100051122      * se profilo di area verifico se area di appartenenza
041200051122     c                   if        *in80
041300051122     c                   if        orgcar <> o36car
041400051122     c                   seton                                        9396
041500051122     c                   goto      endcontr
041600051122     c                   endif
041700051122     c                   endif
041800051122      * se profilo di distretto verifico se di appartenenza
041900051122     c                   if        *in81
042000051122     c                   if        orgfl3 <> o36dis
042100051122     c                   seton                                        9296
042200051122     c                   goto      endcontr
042300051122     c                   endif
042400051122     c                   endif
042500051122      *
042600031105     c                   endif
042700051122      * controllo congruenza selezioni a video
042800051122     c                   if        v1cfil > 0 and
042900051122     c                             (varea <> *blank or vdist <> *blank)
043000051122     c                   seton                                        9596
043100051122     c                   goto      endcontr
043200051122     c                   endif
043300051122      *
043400051122     c                   if        varea <> *blank and vdist <> *blank
043500051122     c                   seton                                        9496
043600051122     c                   goto      endcontr
043700051122     c                   endif
043800051121      * x sede parzializza area o distretto
043900051121     C     varea         ifeq      '?'
044000051121     C                   movel     '05'          kcod
044100051121     C                   clear                   kkey
044200051121     C                   call      'X§TABER'
044300051121     C                   parm                    kkut
044400051121     C                   parm                    kcod
044500051121     C                   parm                    kkey
044600051121     C                   parm                    §des             30
044700051121     c                   movel     §des          varead
044800051121     c                   movel     kkey          varea
044900051121     C*
045000051121     C                   else
045100051121     C*
045200051121     C                   if        varea <> *blank and varea <> *zero
045300051121     C                   movel     '05'          kcod
045400051121     C                   movel(p)  varea         kkey
045500051121     C     ktab          chain     tabel00f
045600051121     c                   if        %found(tabel00f) and tblflg = *blank
045700051121     C                   movel     tbluni        varead
045800051121     c                   else
045900051121     c                   movel     *all'?'       varead
046000051121     C                   end
046100051121     C                   end
046200051121     C                   end
046300051121      * distretto
046400051121     C     vdist         ifeq      '?'
046500051121     C                   movel     '17'          kcod
046600051121     C                   clear                   kkey
046700051121     C                   call      'X§TABER'
046800051121     C                   parm                    kkut
046900051121     C                   parm                    kcod
047000051121     C                   parm                    kkey
047100051121     C                   parm                    §des
047200051121     c                   movel     §des          vdistd
047300051121     c                   movel     kkey          vdist
047400051121     C*
047500051121     C                   else
047600051121     C*
047700051121     C                   if        vdist <> *blank and vdist <> *zero
047800051121     C                   movel     '17'          kcod
047900051121     C                   movel(p)  vdist         kkey
048000051121     C     ktab          chain     tabel00f
048100051121     c                   if        %found(tabel00f) and tblflg = *blank
048200051121     C                   movel     tbluni        vdistd
048300051121     c                   else
048400051121     c                   movel     *all'?'       vdistd
048500051121     C                   end
048600051121     C                   end
048700051121     C                   end
048800051121
048900031105     c                   if        *in90 and v1cfil = *zeros
049000031105     c                   seton                                        9996
049100031105     c                   endif
049200040503     c
049300051122     c     endcontr      endsr
049400050802     c*-------------------------------------------------
049500050802     c     comandi       begsr
049600050802     c*-------------------------------------------------
049700050802     C                   close     waitra0f
049800050802      *
049900050802     C                   z-add     66            luncmd
050000050802     C                   CALL      'QCMDEXC'
050100050802     C                   parm                    coman
050200050802     C                   parm                    luncmd
050300050802      *
050400050802     C  nkc              OPEN      waitra0f
050500050802     c                   endsr
050600011026     c*-------------------------------------------------
050700011026     c     *inzsr        begsr
050800011026     c*-------------------------------------------------
050900020304     c     *entry        plist
051000020304     c                   parm                    kpjba
051100050801
051200050801     c     ktab          klist
051300050801     c                   kfld                    kkut              1 0
051400050801     c                   kfld                    kcod              2
051500050801     c                   kfld                    kkey              8
051600050801     c                   z-add     1             kkut
051700050801     c                   move      '05'          kcod
051800050801     c
051900050801
052000020218     C                   time                    w0120            14 0
052100020218     C                   move      w0120         wdat              8 0
052200050801     c                   move      wdat          v1cdal
052300050801     c                   sub       10            v1cdal
052400020404     c                   z-add     99            line
052500030630      *profilo di filiale estraggo solo quelli relativi al P.O.
052600030630     C                   CLEAR                   Tibs36Ds
052700030630     C                   EVAL      I36ute = knmus
052800030630     C                   CALL      'TIBS36R'
052900030630     C                   PARM                    tibs36ds
053000051017      *se profilo di filiale per pilotare le letture solo sul P.O
053100050802     c                   clear                   lisarea
053200050802     c                   clear                   lisdist
053300051017     c                   eval      saltp = 'N'
053400051017     c                   eval      vdet  = 'S'
053500051017      * x filiale duplica file di work in qtemp
053600051017     c                   movea     cmd2          coman
053700051017     c                   exsr      comandi
053800030630     c                   if        o36pou <> 046
053900050805     c                   move      o36pou        v1cfil
054000050802      * verifica se profili di filiale con autorizzazione a area o distretto
054100050802     c                   if        %subst(knmus:1:3) ='REA'
054200050802     c                   seton                                        80
054300050802     c                   move      'S'           lisarea
054400050802     c                   endif
054500050802     c
054600050802     c                   if        %subst(knmus:1:3) ='RED'
054700050802     c                   seton                                        81
054800050802     c                   move      'S'           lisdist
054900050802     c                   endif
055000051018     c                   else
055100051018      *profilo di sede
055200051018     c                   seton                                        82
055300030630     c                   endif
055400050802
055500050805     c                   if        v1cfil <> 0
055600031105     c     v1cfil        chain     azorg01l
055700031105     c                   if        %found(azorg01l)
055800031105     c                   movel     orgdes        v1dfil
055900050802     c                   move      orgcar        warea
056000050802     c                   move      orgfl3        wdist
056100031105     c                   endif
056200050805     c                   endif
056300011026     c                   endsr
056400020404     C*--------------------------------------------------------------------
056500050623     Oqsysprt   E            testata        1 02
056600050622     o                                           10 'TRMZ73R'
056700020404     o                       knmus               22
056800050622     o                                        +   1 'Lista automezzi città imma-
056900050622     o                                              tricolati prima del '
057000050623     o                       v1cdal           +   1
057100050623     o                       udate         y  +   3
057200050623     O                                        +   1 'Pag.'
057300020404     O                       page          z  +   1
057400050623     O          E            testata     2  1
057500050623     o                                           16 'Punto Operativo'
057600050623     o                                           35 'Tot.Mezzi'
057700051122     o                                           47 'Tot.Dopo"'
057800051121     o                       anno2               49
057900051122     o                                           62 'Tot.Prima"'
058000051121     o                       anno2               64
058100050623
058200031105     O          E            filiale     1
058300050623     o                       orgfil               4
058400050623     o                       orgdes              25
058500050623     o                       totmzfil      z     35
058600050623     o                       totokfil      z     47
058700050623     o                       totoldfil     z     60
058800050623
058900050623     O          E            area        2  1
059000050623     o                                              'Totale area'
059100050623     o                       excar            +   1
059200050801     o                       exaread          +   1
059300050623     o                       totmzarea     z     35
059400050623     o                       totokarea     z     47
059500050623     o                       totoldarea    z     60
059600050623     O          E            dist        2
059700050623     o                                              'Totale distretto'
059800050623     o                       exfl3            +   1
059900050623     o                       totmzdis      z     35
060000050623     o                       totokdis      z     47
060100050623     o                       totolddis     z     60
060200050623     O          E            azienda     2
060300050623     o                                              'Totale azienda'
060400050623     o                       totmzaz       z     35
060500050623     o                       totokaz       z     47
060600050623     o                       totoldaz      z     60
060700050802      *---------------------------------------------------------
060800050802     Oqsysprt   E            testafil       1 02
060900050802     o                                           10 'TRMZ73R'
061000050802     o                       knmus               22
061100050802     o                                        +   1 'Lista automezzi città imma-
061200050802     o                                              tricolati prima del '
061300050802     o                       v1cdal           +   1
061400050802     o                       udate         y  +   3
061500050802     O                                        +   1 'Pag.'
061600050802     O                       page          z  +   1
061700050803     O          E            testafil    2  1
061800050803     o                                           16 'Societa'
061900050803     o                                           42 'Autista'
062000050803     o                                           66 'Mezzo'
062100050803     o                                           84 'Targa'
062200050803     o                                           92 'Anno'
062300081204     o                                          106 'Allestimento'
062400050802     O          E            detfil      1
062500051122     o                       TRASOC           +   9
062600050802     o                       TRAAUT           +   1
062700050802     o                       TRATIA           +   1
062800050802     o                       TRASCO           +   1
062900050802     o                       TRATAA           +   1
063000050802     o                       TRAANNO          +   1
063100081204     o                       TRAfur           +   1
063200081204     o                       TRAfurd          +   1
063300050803     O          E            totfil      1  1
063400050803     o                                              'Totale P.O '
063500050803     o                       exfil            +   1
063600051121     o                       orgdes           +   1
063700051122     o                       totmzfil      z  +   4
063800050803     O          E            detarea     1  1
063900050803     o                                              'Totale area'
064000050803     o                       excar            +   1
064100051121     o                       exaread20        +   1
064200051122     o                       totmzarea     z  +   4
064300050803     O          E            detdist     1
064400050803     o                                              'Totale distretto'
064500050803     o                       exfl3            +   1
064600051121     o                       exdistd20        +   1
064700051121     o                       totmzdis      z  +   1
064800051018     O          E            detaz       1
064900051122     o                                              'Totale azienda'
065000051122     o                       totmzaz       z     45
065100050802**  CMD1
065200050802CLRPFM    FILE(WAITRA0F)
065300050802**  CMD2
065400050802CRTDUPOBJ OBJ(WAITRA0F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
065500050802**  CMD3
065600050802DLTF FILE(QTEMP/WAITRA0F)
