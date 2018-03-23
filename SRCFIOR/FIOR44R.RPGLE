000100040422     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200040422
000300040422      *------------------------------------------------------------------------*
000400040422      *                                                                        *
000500040422      *            STATISTICHE ORM FISSI/MANUALI                               *
000600040422      *                                                                        *
000700040422      *------------------------------------------------------------------------*
000800040422
000900040422     fazorg01l  if   e           k disk
001000040422     ftabel00f  if   e           k disk
001100040422     fazcln01l  if   e           k disk
001200040422     ffior44d   cf   e             workstn
001300040422
001400040422      *------------------------------------------------------------------------*
001500040422      *   S C H I E R E
001600040422      *------------------------------------------------------------------------*
001700040422     d skpoel          s              3  0 dim(250)                             po da elab.
001800130517     d msg             s             78    dim(12) ctdata perrcd(1)             msg video
001900040422
002000040422      *------------------------------------------------------------------------*
002100040422      *   V A R I A B I L I
002200040422      *------------------------------------------------------------------------*
002300040422     d datamsg         s             10
002400040422     d dataoggi        s              8  0
002500040422     d dtpuliaammgg    s              8  0
002600040422     d dtpuliggmmaa    s              8  0
002700040422     d kann            s                   like(clnann)
002800040422     d kcod            s                   like(tblcod)
002900040422     d kkey            s                   like(tblkey)
003000040422     d kmes            s                   like(clnmes)
003100040422     d ktfa            s                   like(clntfa)
003200040422     d ktfp            s                   like(clntfp)
003300040422     d sav_g02tgi      s                   like(g02tgi)
003400040422     d xa              s              5i 0 inz(*zeros)                          ind.sk po da elab.
003500040422     d xb              s              5i 0 inz(*zeros)                          ind.sk po TRUL26
003600040422     d wcdtda          s              8  0
003700040422     d wcdtal          s              8  0
003800040422     d wdataal         s               d   datfmt(*iso)
003900040422     d wdatada         s               d   datfmt(*iso)
004000040422     d wdtgio          s              8  0
004100040422     d wgiolav         s              5  0
004200040422     d wgiorno         s              2  0
004300040422     d w0030           s              3  0
004400040422     d w0140           s             14  0
004500040422
004600040422      *------------------------------------------------------------------------*
004700040422      *   D S   I N T E R N E / E S T E R N E
004800040422      *------------------------------------------------------------------------*
004900040422     d clnmat          ds
005000040422     d  mat                    1     31    dim(31)
005100040422     d clnpom          ds
005200040422     d  pom                    1     31    dim(31)
005300040422
005400040422     d datapul         ds                  inz
005500040422     d  giorno                 1      2
005600040422     d  mese                   3      4
005700040422     d  anno                   5      8
005800040422
005900040422     d wgidat          ds                  inz
006000040422     d  giodat                 1      8  0
006100040422     d  gioinv                 9     16  0
006200040422     d  giotgi                17     21  0
006300040422
006400040422     d wlbdat          ds                  inz
006500040422     d  g02dat                 1      8  0
006600040422     d  g02inv                 9     16  0
006700040422     d  g02err                17     17
006800040422     d  g02tgi                18     22  0
006900040422
007000040422     d cncr80        e ds
007100040422     d ds5a2         e ds
007200040422     d fior44ds      e ds
007300040422     d kpjba         e ds
007400040422     d trul26ds      e ds
007500040422     d  skpo                  11    760  0 dim(250)
007600040422     d  skpot                761   1510  0 dim(250)
007700040422     d ut§dse0f      e ds
007800040422     d                sds
007900040422     d  vtcpgm                 1     10
008000040422
008100040422      *------------------------------------------------------------------------*
008200040422      *   C O S A T A N T I
008300040422      *------------------------------------------------------------------------*
008400040422      *  titolo videata (lunghezza massima 34)
008500040422     d tit_a           c                   const('***  Stampa Statistiche O.R.M-
008600040422     d                                     . ***')
008700040422
008800040422      *------------------------------------------------------------------------*
008900040422      *  RIEPILOGO INDICATORI
009000040422      *------------------------------------------------------------------------*
009100040422      * 06 - P.O. da elaborare protetto
009200040422      * 28 - ERRORE GENERICO DSPF
009300040422      * 30 - Comodo
009400040422      * 40 - ERRORE p.o. da elaborare
009500040422      * 41 - ERRORE data elaborazione da
009600040422      * 42 - ERRORE data elaborazione al
009700040422      * 43 - ERRORE scegliere la statistica
009800130522      * 44 - ERRORE immettere la selezione
009900040428      * 45 - ERRORE immettere il n. di orm
010000040428      * 46 - ERRORE cliente codificato
010100130522      * 47 - ERRORE ordinamento
010200040422      *------------------------------------------------------------------------*
010300040422
010400040422      * pulizia Videata
010500040422     c                   If        Not *In28
010600040422     c                   ExSr      Sr_Pulvid
010700040422     c                   EndIf
010800040422
010900040422      * emetto videata
011000040422     c                   Do        *Hival
011100040422
011200040422     c                   Exfmt     or44d01
011300040422
011400040422      * F3=Fine
011500040422     c   kc              Leave
011600040422     c                   Setoff                                       28
011700040422
011800040422      * controlli
011900040422     c                   ExSr      Sr_Contr
012000040422
012100040422      * F6=Elabora
012200040422     c                   If        *InKf and Not *In28
012300040422     c                   ExSr      Sr_Contr
012400040422     c  n28              ExSr      Sr_Elabora
012500040422     c  n28              Leave
012600040422     c                   EndIf
012700040422
012800040422     c   28              Iter
012900040422
013000040422     c                   EndDo
013100040422
013200040422     c                   Eval      *InLr = *On
013300040422      *------------------------------------------------------------------------*
013400040422      * - PULIZIA VIDEATA
013500040422      *------------------------------------------------------------------------*
013600040422     c     Sr_Pulvid     BegSr
013700040422
013800040422     c                   Clear                   v1cdtda
013900040422     c                   Clear                   v1cdtal
014000040422     c                   Clear                   v1cfis
014100040422     c                   Clear                   v1cman
014200130522     c                   Clear                   v1csel
014300040428     c                   Clear                   v1ccod
014400040428     c                   Clear                   v1corm
014500130522     c                   eval      V1Cord = 'R'
014600040422
014700040422     c                   EndSr
014800040422      *------------------------------------------------------------------------*
014900040422      * - CONTROLLO I DATI DELLA VIDEATA
015000040422      *------------------------------------------------------------------------*
015100040422     c     Sr_Contr      BegSr
015200040422
015300040422      * reset indicatori
015400040422     c                   Setoff                                       404142
015500040426     c                   Setoff                                       4344
015600040422
015700040422      * p.o. da elaborare
015800040422     c                   ExSr      Sr_Contrpo
015900040422     c                   If        *In28
016000040422     c                   Goto      endsrcontr
016100040422     c                   EndIf
016200040422
016300040422      * date
016400040422     c                   ExSr      Sr_Contrdate
016500040422     c                   If        *In28
016600040422     c                   Goto      endsrcontr
016700040422     c                   EndIf
016800040422      * statistica
016900040426     c                   If        v1cfis = *Blanks and v1cman = *Blanks
017000040422     c                   Eval      *In28 = *On
017100040422     c                   Eval      *In43 = *On
017200040422     c                   Eval      v1cmsg = msg(8)
017300040422     c                   Goto      endsrcontr
017400040422     c                   EndIf
017500040428
017600040428      * statistica ORM Telefonici
017700040428if  1c                   If        v1cman <> *Blanks
017800130522      * selezione
017900130522     c                   If        v1csel = *Blanks
018000040422     c                   Eval      *In28 = *On
018100040422     c                   Eval      *In44 = *On
018200040422     c                   Eval      v1cmsg = msg(9)
018300040422     c                   Goto      endsrcontr
018400040422     c                   EndIf
018500040428      * numero orm
018600040428     c                   If        v1corm = *Zeros
018700040428     c                   Eval      *In28 = *On
018800040428     c                   Eval      *In45 = *On
018900040428     c                   Eval      v1cmsg = msg(10)
019000040428     c                   Goto      endsrcontr
019100040428     c                   EndIf
019200130522      * se ordinata per commerciale SOLO codificati o TUTTI
019300130522     c                   IF        V1Cord = 'U' and V1Ccod = 'N'
019400130517     c                   eval      *in28 = *on
019500130517     c                   eval      *in47 = *on
019600130517     c                   eval      V1Cmsg = msg(12)
019700130517     c                   leavesr
019800130517     c                   ENDIF
019900040428
020000040428   x1c                   Else
020100040428
020200130522     c                   If        v1csel <> *Blanks
020300040426     c                   Eval      *In28 = *On
020400040426     c                   Eval      *In44 = *On
020500040428     c                   Eval      v1cmsg = msg(11)
020600040426     c                   Goto      endsrcontr
020700040426     c                   EndIf
020800040428     c                   If        v1ccod <> *Blanks
020900040428     c                   Eval      *In28 = *On
021000040428     c                   Eval      *In46 = *On
021100040428     c                   Eval      v1cmsg = msg(11)
021200040428     c                   Goto      endsrcontr
021300040428     c                   EndIf
021400130515     c                   If        v1corm <> *zeros
021500040428     c                   Eval      *In28 = *On
021600040428     c                   Eval      *In45 = *On
021700040428     c                   Eval      v1cmsg = msg(11)
021800040428     c                   Goto      endsrcontr
021900040428     c                   EndIf
022000040428
022100040428e   1c                   EndIf
022200040422
022300040422     c     endsrcontr    EndSr
022400040422      *------------------------------------------------------------------------*
022500040422      * - CONTROLLO IL P.O. DA ELABORARE
022600040422      *------------------------------------------------------------------------*
022700040422     c     Sr_Contrpo    BegSr
022800040422
022900040422     c                   Setoff                                       40
023000040422     c                   Clear                   v1dpoel
023100040422
023200040422     c                   If        v1cpoel = 999
023300040422     c                   Clear                   v1dpoel
023400040422     c                   Eval      v1dpoel = 'Tutti i p.o. dell''AS'
023500040422     c                   Goto      endsrctpo
023600040422     c                   EndIf
023700040422
023800040422      * non deve essere a zero
023900040422     c                   If        v1cpoel = *zeros
024000040422     c                   Seton                                        40  28
024100040422     c                   Movel     msg(1)        v1cmsg
024200040422     c                   Goto      endsrctpo
024300040422     c                   EndIf
024400040422
024500040422      * deve essere caricato nella sk
024600040422     c     v1cpoel       Lookup    skpoel                                 30
024700040422     c                   If        *in30 = *off
024800040422     c                   Seton                                        40  28
024900040422     c                   Movel     msg(1)        v1cmsg
025000040422     c                   Else
025100040422
025200040422      * decodifica e controlla la validità
025300040422     c     v1cpoel       Chain     azorg01L
025400040422     c                   If        not%found(azorg01l) or
025500040422     c                             ORGfva <> *blanks
025600040422     c                   Seton                                        40  28
025700040422     c                   Movel     msg(1)        v1cmsg
025800040422     c                   Else
025900040422     c                   If        orgfag <> 'A' and orgfag <> 'F'
026000040422     c                   Seton                                        40  28
026100040422     c                   Movel     msg(1)        v1cmsg
026200040422     c                   EndIf
026300040422     c                   Movel     orgDES        v1dpoel
026400040422     c                   EndIf
026500040422     c                   EndIf
026600040422
026700040422     c     endsrctpo     EndSr
026800040422      *------------------------------------------------------------------------*
026900040422      * - CONTROLLO LE DATE
027000040422      *------------------------------------------------------------------------*
027100040422     c     Sr_Contrdate  BegSr
027200040422
027300040422     c                   Setoff                                       4142
027400040422
027500040422      * data 'da' deve essere impostata
027600040422     c                   If        v1cdtda = *zeros
027700040422     c                   Seton                                        41  28
027800040422     c                   Movel     msg(2)        v1cmsg
027900040422     c                   Goto      endsrctdate
028000040422     c                   EndIf
028100040422
028200040422      * controllo la data inserita 'da'
028300040422     c                   Clear                   wcdtda
028400040422     c                   Clear                   wlbdat
028500040422     c                   Z-add     v1cdtda       G02dat
028600040422     c                   Call      'XSRDA8'
028700040422     c                   Parm                    wlbdat
028800040422     c                   If        G02err = '1'
028900040422     c                   Eval      *in41 = *ON
029000040422     c                   Eval      *in28 = *ON
029100040422     c                   Movel     msg(3)        v1cmsg
029200040422     c                   Goto      endsrctdate
029300040422     c                   Else
029400040422     c                   Z-add     G02dat        v1cdtda
029500040422     c                   Z-add     G02inv        wcdtda
029600040422     c                   EndIf
029700040422
029800040422      * non può essere >= ad oggi
029900040422     c                   If        wcdtda >= dataoggi
030000040422     c                   Seton                                        41  28
030100040422     c                   Movel     msg(4)        v1cmsg
030200040422     c                   Goto      endsrctdate
030300040422     c                   EndIf
030400040422
030500040422      * deve essere > della data di pulizia ORM
030600040422     c                   If        wcdtda <= dtpuliaammgg
030700040422     c                   Seton                                        41  28
030800040422     c                   Movel     msg(5)        v1cmsg
030900040422     c                   Movel     dtpuliggmmaa  datapul
031000040422     c                   Eval      datamsg = %trim (giorno + '/' +
031100040422     c                             mese + '/' + anno)
031200040422     c                   Eval      %subst(v1cmsg: 62: 10) = datamsg
031300040422     c                   Goto      endsrctdate
031400040422     c                   EndIf
031500040422
031600040422      * deve essere un giorno feriale
031700040422     c     *iso          Movel     wcdtda        wdatada
031800040422     c                   Extrct    wdatada:*y    kann
031900040422     c                   Extrct    wdatada:*m    kmes
032000040422     c                   Extrct    wdatada:*d    wgiorno
032100040422     c                   Clear                   ktfa
032200040422     c                   If        v1cpoel = 999
032300040422     c                   Movel     simfel        ktfp
032400040422     c                   Else
032500040422     c                   Eval      ktfp = v1cpoel
032600040422     c                   EndIf
032700040422     c     kazcln        Chain     azcln01l
032800040422     c                   If        %found(azcln01l) and
032900040422     c                             (MAT(wgiorno) =  'F'
033000040422     c                                    or  POM(wgiorno) =  'F')
033100040422     c                   Seton                                        41  28
033200040422     c                   Movel     msg(6)        v1cmsg
033300040422     c                   Goto      endsrctdate
033400040422     c                   EndIf
033500040422
033600040422      * data 'al' se non impostata è = a data 'da'
033700040422     c                   If        v1cdtal = *zeros
033800040422     c                   Eval      v1cdtal = v1cdtda
033900040422     c                   Eval      wcdtal = wcdtda
034000040422     c                   Goto      endsrctdate
034100040422
034200040422     c                   Else
034300040422
034400040422      * controllo la data inserita 'al'
034500040422     c                   Clear                   wcdtal
034600040422     c                   Clear                   wlbdat
034700040422     c                   Z-add     v1cdtal       G02dat
034800040422     c                   Call      'XSRDA8'
034900040422     c                   Parm                    wlbdat
035000040422     c                   If        G02err = '1'
035100040422     c                   Eval      *in42 = *ON
035200040422     c                   Eval      *in28 = *ON
035300040422     c                   Movel     msg(3)        v1cmsg
035400040422     c                   Goto      endsrctdate
035500040422     c                   Else
035600040422     c                   Z-add     G02dat        v1cdtal
035700040422     c                   Z-add     G02inv        wcdtal
035800040422     c                   EndIf
035900040422
036000040422      * non può essere >= ad oggi
036100040422     c                   If        wcdtal >= dataoggi
036200040422     c                   Seton                                        42  28
036300040422     c                   Movel     msg(4)        v1cmsg
036400040422     c                   Goto      endsrctdate
036500040422     c                   EndIf
036600040422
036700040422      * deve essere > della data di pulizia ORM
036800040422     c                   If        wcdtal <= dtpuliaammgg
036900040422     c                   Seton                                        42  28
037000040422     c                   Movel     msg(5)        v1cmsg
037100040422     c                   Movel     dtpuliggmmaa  datapul
037200040422     c                   Eval      datamsg = %trim (giorno + '/' +
037300040422     c                             mese + '/' + anno)
037400040422     c                   Eval      %subst(v1cmsg: 62: 10) = datamsg
037500040422     c                   Goto      endsrctdate
037600040422     c                   EndIf
037700040422
037800040422      * deve essere un giorno feriale
037900040422     c     *iso          Movel     wcdtal        wdataal
038000040422     c                   Extrct    wdataal:*y    kann
038100040422     c                   Extrct    wdataal:*m    kmes
038200040422     c                   Extrct    wdataal:*d    wgiorno
038300040422     c                   Clear                   ktfa
038400040422     c                   If        v1cpoel = 999
038500040422     c                   Movel     simfel        ktfp
038600040422     c                   Else
038700040422     c                   Eval      ktfp = v1cpoel
038800040422     c                   EndIf
038900040422     c     kazcln        Chain     azcln01l
039000040422     c                   If        %found(azcln01l) and
039100040422     c                             (MAT(wgiorno) =  'F'
039200040422     c                                    or  POM(wgiorno) =  'F')
039300040422     c                   Seton                                        42  28
039400040422     c                   Movel     msg(6)        v1cmsg
039500040422     c                   Goto      endsrctdate
039600040422     c                   EndIf
039700040422
039800040422      * range
039900040422     c                   If        wcdtda > wcdtal
040000040422     c                   Seton                                        41  28
040100040422     c                   Movel     msg(3)        v1cmsg
040200040422     c                   Goto      endsrctdate
040300040422     c                   EndIf
040400040422
040500040422      * range massimo di 31 gg
040600040422     c     wdataal       Subdur    wdatada       wgiolav:*d
040700040422     c                   Eval      wgiolav = wgiolav + 1
040800040422     c                   If        wgiolav > 31
040900040422     c                   Movel     msg(7)        v1cmsg
041000040422     c                   Eval      %Subst(V1cmsg:50:2) = '31'
041100040422     c                   Seton                                        41  28
041200040422     c                   Goto      endsrctdate
041300040422     c                   EndIf
041400040422
041500040422     c                   EndIf
041600040422
041700040422     c     endsrctdate   EndSr
041800040422      *------------------------------------------------------------------------*
041900040422      * - ELABORA
042000040422      *------------------------------------------------------------------------*
042100040422     c     Sr_Elabora    BegSr
042200040422
042300040422     c                   Eval      i44poel = v1cpoel
042400040422
042500040422     c                   Eval      i44dtda = wcdtda
042600040422     c                   Eval      i44dtal = wcdtal
042700040422
042800040422     c                   Eval      i44fis  = v1cfis
042900040428
043000040422     c                   Eval      i44man  = v1cman
043100130522     c                   Eval      i44sel  = v1csel
043200040428     c                   Eval      i44cod  = v1ccod
043300040428     c                   Eval      i44orm  = v1corm
043400130522     c                   eval      I44ord  = V1Cord
043500040422
043600040422     c                   Clear                   kpjbu
043700040422     c                   Movel     fior44ds      kpjbu
043800040422
043900040422      * Lancio statistia ORM fissi
044000040422     c                   If        v1cfis <> *Blanks
044100040422     c                   Move      'OR45'        kcoaz
044200040422     c******             Call      'FIOR45R'
044300040422     c                   Call      'BCH10'
044400040422     c                   Parm                    kpjba
044500040422     c                   EndIf
044600040422
044700040422      * Lancio statistia ORM manuali
044800040428     c                   If        v1cman <> *Blanks
044900040422     c                   Move      'OR46'        kcoaz
045000040422     c                   Call      'BCH10'
045100040422     c                   Parm                    kpjba
045200040422     c                   EndIf
045300040422
045400040422     c                   EndSr
045500040422      *------------------------------------------------------------------------*
045600040422      * - ROUTINE INIZIALE
045700040422      *------------------------------------------------------------------------*
045800040422     c     *INZSR        BegSr
045900040422
046000040422     c     *entry        Plist
046100040422     c                   Parm                    kpjba
046200040422
046300040422     c                   Z-ADD     1             CODUT
046400040422     c                   CALL      'X§PARUT'
046500040422     c                   PARM                    UT§DSE0F
046600040422     c                   MOVEL     RAGUT         RSUT
046700040422     c                   MOVEL     REC80         CNCR80
046800040422
046900040422     c     ktab          klist
047000040422     c                   kfld                    codut
047100040422     c                   kfld                    kcod
047200040422     c                   kfld                    kkey
047300040422
047400040422     c     kazcln        klist
047500040422     c                   kfld                    ktfp
047600040422     c                   kfld                    ktfa
047700040422     c                   kfld                    kann
047800040422     c                   kfld                    kmes
047900040422
048000040422     c                   Movel     tit_a         vtctit
048100040422     c                   Clear                   fior44ds
048200040422     c                   Clear                   v1cpoel
048300040422     c                   Clear                   v1dpoel
048400040422
048500040422     c                   If        simtpp = '2' or simtpp = *blanks
048600040422     c                   Movel     simpou        V1Cpoel
048700040422     c                   Eval      *in06 = *On
048800040422     c                   Else
048900040422     c                   Movel     SIMFEL        V1Cpoel
049000040422     c                   EndIf
049100040422
049200040422     c     V1Cpoel       Chain     azorg01L
049300040422     c                   IF        not%found(azorg01l) or
049400040422     c                             orgfva <> *blanks   or
049500040422     c                             (orgfag <> 'A' and orgfag <> 'F')
049600040422     c                   Eval      *in28 = *on
049700040422     c                   Eval      *in40 = *on
049800040422     c                   Movel     msg(1)        v1cmsg
049900040422     c                   ELSE
050000040422      * decodifico p.o. da elaborare
050100040422     c                   Movel     orgDES        v1dpoel
050200040422     c                   ENDIF
050300040422
050400040422      * reperisce data e ora
050500040422     c                   TIME                    W0140
050600040422      * UDATE IN GGMMAAAA
050700040422     c                   MOVE      W0140         WDTGIO
050800040422      * UDATE IN AAAAMMGG
050900040422     c                   Z-ADD     WDTGIO        G02DAT
051000040422     c                   MOVEL     *BLANK        G02ERR
051100040422     c                   CALL      'XSRDA8'
051200040422     c                   PARM                    WLBDAT
051300040422     c                   Z-ADD     G02INV        dataoggi
051400040422     c                   Z-add     g02tgi        sav_g02tgi
051500040422
051600040422     c                   Z-add     1             xa
051700040422
051800040422      * Ricerco tutti i terminal di partenza
051900040422     c                   Clear                   Trul26Ds
052000040422     c                   Eval      I26tpt = 'P'
052100040422     c                   Eval      I26tla = 'L'
052200040422     c                   Call      'TRUL26R'
052300040422     c                   Parm                    Trul26Ds
052400040422      * carico i p.o. da elaborare dell'AS in un sk
052500040422 b1  c                   Do        250           Xb
052600040422     c                   If        Skpot(Xb) = *Zeros
052700040422     c                   Leave
052800040422     c                   EndIf
052900040422      * devo caricare solo quelli che hanno il terminal di partenza = simfel
053000040422     c                   If        Skpot(Xb) = Simfel
053100040422     c                   Eval      Skpoel(Xa) = Skpo(Xb)
053200040422     c                   Add       1             Xa
053300040422     c                   EndIf
053400040422
053500040422 e1  c                   EndDo
053600040422
053700040422      * per filiale
053800040422     c                   Clear                   ds5a2
053900040422     c                   Movel     '5A'          kcod
054000040422     c                   Movel     '2       '    kkey
054100040422     c     ktab          Chain     tabel00f
054200040422     c                   If        %found(tabel00f) and tblflg = *blanks
054300040422     c                   Movel     tbluni        ds5a2
054400040422     c                   Z-add     §5aorf        W0030
054500040422     c                   EndIf
054600040422
054700040422      * Calcolo data
054800040422     c     sav_g02tgi    Sub       W0030         giotgi
054900040422     c                   Call      'XSRGI8'
055000040422     c                   Parm                    wgidat
055100040422     c                   Z-add     giodat        dtpuliggmmaa
055200040422
055300040422      * Giro la data
055400040422     c                   If        dtpuliggmmaa > 0
055500040422     c                   Move      dtpuliggmmaa  g02dat
055600040422     c                   Movel     *blanks       g02err
055700040422     c                   Call      'XSRDA8'
055800040422     c                   Parm                    wlbdat
055900040422     c                   If        g02err = '1'
056000040422     c                   Clear                   dtpuliggmmaa
056100040422     c                   Else
056200040422     c                   Z-add     g02inv        dtpuliaammgg
056300040422     c                   EndIf
056400040422     c                   EndIf
056500040422
056600040422     c                   EndSr
056700040422      *------------------------------------------------------------------------*
056800040422** MSG  Lungh. 78                                                            *
056900070418Filiale non gestita o annullata                                                     1
057000040422Inserire la data immissione                                                         2
057100040422Data immissione errata                                                              3
057200040422Data immissione deve essere inferiore alla data odierna                             4
057300040422Data immissione deve essere maggiore alla data pulizia ORM                          5
057400040422Data immissione deve essere un giorno feriale                                       6
057500040422Non è possibile elaborare un periodo superiore a xx giorni                          7
057600040422Scegliere quale statistica elaborare                                                8
057700130522Immettere la selezione                                                              9
057800040428Immettere il numero di ORM                                                          10
057900040428Immettere il dato solo se richiesta la statistica ORM Telefonici                    11
058000130522Ordinamento per commerciale NON possibile per clienti NON CODIFICATI!               12
