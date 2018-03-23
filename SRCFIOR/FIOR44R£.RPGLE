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
001800040428     d msg             s             78    dim(11) ctdata perrcd(1)             msg video
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
009800040422      * 44 - ERRORE immettere l'ordinamento
009900040428      * 45 - ERRORE immettere il n. di orm
010000040428      * 46 - ERRORE cliente codificato
010100040422      *------------------------------------------------------------------------*
010200040422
010300040422      * pulizia Videata
010400040422     c                   If        Not *In28
010500040422     c                   ExSr      Sr_Pulvid
010600040422     c                   EndIf
010700040422
010800040422      * emetto videata
010900040422     c                   Do        *Hival
011000040422
011100040422     c                   Exfmt     or44d01
011200040422
011300040422      * F3=Fine
011400040422     c   kc              Leave
011500040422     c                   Setoff                                       28
011600040422
011700040422      * controlli
011800040422     c                   ExSr      Sr_Contr
011900040422
012000040422      * F6=Elabora
012100040422     c                   If        *InKf and Not *In28
012200040422     c                   ExSr      Sr_Contr
012300040422     c  n28              ExSr      Sr_Elabora
012400040422     c  n28              Leave
012500040422     c                   EndIf
012600040422
012700040422     c   28              Iter
012800040422
012900040422     c                   EndDo
013000040422
013100040422     c                   Eval      *InLr = *On
013200040422      *------------------------------------------------------------------------*
013300040422      * - PULIZIA VIDEATA
013400040422      *------------------------------------------------------------------------*
013500040422     c     Sr_Pulvid     BegSr
013600040422
013700040422     c                   Clear                   v1cdtda
013800040422     c                   Clear                   v1cdtal
013900040422     c                   Clear                   v1cfis
014000040422     c                   Clear                   v1cman
014100040422     c                   Clear                   v1cord
014200040428     c                   Clear                   v1ccod
014300040428     c                   Clear                   v1corm
014400040422
014500040422     c                   EndSr
014600040422      *------------------------------------------------------------------------*
014700040422      * - CONTROLLO I DATI DELLA VIDEATA
014800040422      *------------------------------------------------------------------------*
014900040422     c     Sr_Contr      BegSr
015000040422
015100040422      * reset indicatori
015200040422     c                   Setoff                                       404142
015300040426     c                   Setoff                                       4344
015400040422
015500040422      * p.o. da elaborare
015600040422     c                   ExSr      Sr_Contrpo
015700040422     c                   If        *In28
015800040422     c                   Goto      endsrcontr
015900040422     c                   EndIf
016000040422
016100040422      * date
016200040422     c                   ExSr      Sr_Contrdate
016300040422     c                   If        *In28
016400040422     c                   Goto      endsrcontr
016500040422     c                   EndIf
016600040422      * statistica
016700040426     c                   If        v1cfis = *Blanks and v1cman = *Blanks
016800040422     c                   Eval      *In28 = *On
016900040422     c                   Eval      *In43 = *On
017000040422     c                   Eval      v1cmsg = msg(8)
017100040422     c                   Goto      endsrcontr
017200040422     c                   EndIf
017300040428
017400040428      * statistica ORM Telefonici
017500040428if  1c                   If        v1cman <> *Blanks
017600040428      * ordinamento
017700040428     c                   If        v1cord = *Blanks
017800040422     c                   Eval      *In28 = *On
017900040422     c                   Eval      *In44 = *On
018000040422     c                   Eval      v1cmsg = msg(9)
018100040422     c                   Goto      endsrcontr
018200040422     c                   EndIf
018300040428      * numero orm
018400040428     c                   If        v1corm = *Zeros
018500040428     c                   Eval      *In28 = *On
018600040428     c                   Eval      *In45 = *On
018700040428     c                   Eval      v1cmsg = msg(10)
018800040428     c                   Goto      endsrcontr
018900040428     c                   EndIf
019000040428
019100040428   x1c                   Else
019200040428
019300040428     c                   If        v1cord <> *Blanks
019400040426     c                   Eval      *In28 = *On
019500040426     c                   Eval      *In44 = *On
019600040428     c                   Eval      v1cmsg = msg(11)
019700040426     c                   Goto      endsrcontr
019800040426     c                   EndIf
019900040428     c                   If        v1ccod <> *Blanks
020000040428     c                   Eval      *In28 = *On
020100040428     c                   Eval      *In46 = *On
020200040428     c                   Eval      v1cmsg = msg(11)
020300040428     c                   Goto      endsrcontr
020400040428     c                   EndIf
020500040428     c                   If        v1cord <> *Blanks
020600040428     c                   Eval      *In28 = *On
020700040428     c                   Eval      *In45 = *On
020800040428     c                   Eval      v1cmsg = msg(11)
020900040428     c                   Goto      endsrcontr
021000040428     c                   EndIf
021100040428
021200040428e   1c                   EndIf
021300040422
021400040422     c     endsrcontr    EndSr
021500040422      *------------------------------------------------------------------------*
021600040422      * - CONTROLLO IL P.O. DA ELABORARE
021700040422      *------------------------------------------------------------------------*
021800040422     c     Sr_Contrpo    BegSr
021900040422
022000040422     c                   Setoff                                       40
022100040422     c                   Clear                   v1dpoel
022200040422
022300040422     c                   If        v1cpoel = 999
022400040422     c                   Clear                   v1dpoel
022500040422     c                   Eval      v1dpoel = 'Tutti i p.o. dell''AS'
022600040422     c                   Goto      endsrctpo
022700040422     c                   EndIf
022800040422
022900040422      * non deve essere a zero
023000040422     c                   If        v1cpoel = *zeros
023100040422     c                   Seton                                        40  28
023200040422     c                   Movel     msg(1)        v1cmsg
023300040422     c                   Goto      endsrctpo
023400040422     c                   EndIf
023500040422
023600040422      * deve essere caricato nella sk
023700040422     c     v1cpoel       Lookup    skpoel                                 30
023800040422     c                   If        *in30 = *off
023900040422     c                   Seton                                        40  28
024000040422     c                   Movel     msg(1)        v1cmsg
024100040422     c                   Else
024200040422
024300040422      * decodifica e controlla la validità
024400040422     c     v1cpoel       Chain     azorg01L
024500040422     c                   If        not%found(azorg01l) or
024600040422     c                             ORGfva <> *blanks
024700040422     c                   Seton                                        40  28
024800040422     c                   Movel     msg(1)        v1cmsg
024900040422     c                   Else
025000040422     c                   If        orgfag <> 'A' and orgfag <> 'F'
025100040422     c                   Seton                                        40  28
025200040422     c                   Movel     msg(1)        v1cmsg
025300040422     c                   EndIf
025400040422     c                   Movel     orgDES        v1dpoel
025500040422     c                   EndIf
025600040422     c                   EndIf
025700040422
025800040422     c     endsrctpo     EndSr
025900040422      *------------------------------------------------------------------------*
026000040422      * - CONTROLLO LE DATE
026100040422      *------------------------------------------------------------------------*
026200040422     c     Sr_Contrdate  BegSr
026300040422
026400040422     c                   Setoff                                       4142
026500040422
026600040422      * data 'da' deve essere impostata
026700040422     c                   If        v1cdtda = *zeros
026800040422     c                   Seton                                        41  28
026900040422     c                   Movel     msg(2)        v1cmsg
027000040422     c                   Goto      endsrctdate
027100040422     c                   EndIf
027200040422
027300040422      * controllo la data inserita 'da'
027400040422     c                   Clear                   wcdtda
027500040422     c                   Clear                   wlbdat
027600040422     c                   Z-add     v1cdtda       G02dat
027700040422     c                   Call      'XSRDA8'
027800040422     c                   Parm                    wlbdat
027900040422     c                   If        G02err = '1'
028000040422     c                   Eval      *in41 = *ON
028100040422     c                   Eval      *in28 = *ON
028200040422     c                   Movel     msg(3)        v1cmsg
028300040422     c                   Goto      endsrctdate
028400040422     c                   Else
028500040422     c                   Z-add     G02dat        v1cdtda
028600040422     c                   Z-add     G02inv        wcdtda
028700040422     c                   EndIf
028800040422
028900040422      * non può essere >= ad oggi
029000040422     c                   If        wcdtda >= dataoggi
029100040422     c                   Seton                                        41  28
029200040422     c                   Movel     msg(4)        v1cmsg
029300040422     c                   Goto      endsrctdate
029400040422     c                   EndIf
029500040422
029600040422      * deve essere > della data di pulizia ORM
029700040422     c                   If        wcdtda <= dtpuliaammgg
029800040422     c                   Seton                                        41  28
029900040422     c                   Movel     msg(5)        v1cmsg
030000040422     c                   Movel     dtpuliggmmaa  datapul
030100040422     c                   Eval      datamsg = %trim (giorno + '/' +
030200040422     c                             mese + '/' + anno)
030300040422     c                   Eval      %subst(v1cmsg: 62: 10) = datamsg
030400040422     c                   Goto      endsrctdate
030500040422     c                   EndIf
030600040422
030700040422      * deve essere un giorno feriale
030800040422     c     *iso          Movel     wcdtda        wdatada
030900040422     c                   Extrct    wdatada:*y    kann
031000040422     c                   Extrct    wdatada:*m    kmes
031100040422     c                   Extrct    wdatada:*d    wgiorno
031200040422     c                   Clear                   ktfa
031300040422     c                   If        v1cpoel = 999
031400040422     c                   Movel     simfel        ktfp
031500040422     c                   Else
031600040422     c                   Eval      ktfp = v1cpoel
031700040422     c                   EndIf
031800040422     c     kazcln        Chain     azcln01l
031900040422     c                   If        %found(azcln01l) and
032000040422     c                             (MAT(wgiorno) =  'F'
032100040422     c                                    or  POM(wgiorno) =  'F')
032200040422     c                   Seton                                        41  28
032300040422     c                   Movel     msg(6)        v1cmsg
032400040422     c                   Goto      endsrctdate
032500040422     c                   EndIf
032600040422
032700040422      * data 'al' se non impostata è = a data 'da'
032800040422     c                   If        v1cdtal = *zeros
032900040422     c                   Eval      v1cdtal = v1cdtda
033000040422     c                   Eval      wcdtal = wcdtda
033100040422     c                   Goto      endsrctdate
033200040422
033300040422     c                   Else
033400040422
033500040422      * controllo la data inserita 'al'
033600040422     c                   Clear                   wcdtal
033700040422     c                   Clear                   wlbdat
033800040422     c                   Z-add     v1cdtal       G02dat
033900040422     c                   Call      'XSRDA8'
034000040422     c                   Parm                    wlbdat
034100040422     c                   If        G02err = '1'
034200040422     c                   Eval      *in42 = *ON
034300040422     c                   Eval      *in28 = *ON
034400040422     c                   Movel     msg(3)        v1cmsg
034500040422     c                   Goto      endsrctdate
034600040422     c                   Else
034700040422     c                   Z-add     G02dat        v1cdtal
034800040422     c                   Z-add     G02inv        wcdtal
034900040422     c                   EndIf
035000040422
035100040422      * non può essere >= ad oggi
035200040422     c                   If        wcdtal >= dataoggi
035300040422     c                   Seton                                        42  28
035400040422     c                   Movel     msg(4)        v1cmsg
035500040422     c                   Goto      endsrctdate
035600040422     c                   EndIf
035700040422
035800040422      * deve essere > della data di pulizia ORM
035900040422     c                   If        wcdtal <= dtpuliaammgg
036000040422     c                   Seton                                        42  28
036100040422     c                   Movel     msg(5)        v1cmsg
036200040422     c                   Movel     dtpuliggmmaa  datapul
036300040422     c                   Eval      datamsg = %trim (giorno + '/' +
036400040422     c                             mese + '/' + anno)
036500040422     c                   Eval      %subst(v1cmsg: 62: 10) = datamsg
036600040422     c                   Goto      endsrctdate
036700040422     c                   EndIf
036800040422
036900040422      * deve essere un giorno feriale
037000040422     c     *iso          Movel     wcdtal        wdataal
037100040422     c                   Extrct    wdataal:*y    kann
037200040422     c                   Extrct    wdataal:*m    kmes
037300040422     c                   Extrct    wdataal:*d    wgiorno
037400040422     c                   Clear                   ktfa
037500040422     c                   If        v1cpoel = 999
037600040422     c                   Movel     simfel        ktfp
037700040422     c                   Else
037800040422     c                   Eval      ktfp = v1cpoel
037900040422     c                   EndIf
038000040422     c     kazcln        Chain     azcln01l
038100040422     c                   If        %found(azcln01l) and
038200040422     c                             (MAT(wgiorno) =  'F'
038300040422     c                                    or  POM(wgiorno) =  'F')
038400040422     c                   Seton                                        42  28
038500040422     c                   Movel     msg(6)        v1cmsg
038600040422     c                   Goto      endsrctdate
038700040422     c                   EndIf
038800040422
038900040422      * range
039000040422     c                   If        wcdtda > wcdtal
039100040422     c                   Seton                                        41  28
039200040422     c                   Movel     msg(3)        v1cmsg
039300040422     c                   Goto      endsrctdate
039400040422     c                   EndIf
039500040422
039600040422      * range massimo di 31 gg
039700040422     c     wdataal       Subdur    wdatada       wgiolav:*d
039800040422     c                   Eval      wgiolav = wgiolav + 1
039900040422     c                   If        wgiolav > 31
040000040422     c                   Movel     msg(7)        v1cmsg
040100040422     c                   Eval      %Subst(V1cmsg:50:2) = '31'
040200040422     c                   Seton                                        41  28
040300040422     c                   Goto      endsrctdate
040400040422     c                   EndIf
040500040422
040600040422     c                   EndIf
040700040422
040800040422     c     endsrctdate   EndSr
040900040422      *------------------------------------------------------------------------*
041000040422      * - ELABORA
041100040422      *------------------------------------------------------------------------*
041200040422     c     Sr_Elabora    BegSr
041300040422
041400040422     c                   Eval      i44poel = v1cpoel
041500040422
041600040422     c                   Eval      i44dtda = wcdtda
041700040422     c                   Eval      i44dtal = wcdtal
041800040422
041900040422     c                   Eval      i44fis  = v1cfis
042000040428
042100040422     c                   Eval      i44man  = v1cman
042200040422     c                   Eval      i44ord  = v1cord
042300040428     c                   Eval      i44cod  = v1ccod
042400040428     c                   Eval      i44orm  = v1corm
042500040422
042600040422     c                   Clear                   kpjbu
042700040422     c                   Movel     fior44ds      kpjbu
042800040422
042900040422      * Lancio statistia ORM fissi
043000040422     c                   If        v1cfis <> *Blanks
043100040422     c                   Move      'OR45'        kcoaz
043200040422     c******             Call      'FIOR45R'
043300040422     c                   Call      'BCH10'
043400040422     c                   Parm                    kpjba
043500040422     c                   EndIf
043600040422
043700040422      * Lancio statistia ORM manuali
043800040428     c                   If        v1cman <> *Blanks
043900040422     c                   Move      'OR46'        kcoaz
044000040422     c******             Call      'FIOR46R'
044100040422     c                   Call      'BCH10'
044200040422     c                   Parm                    kpjba
044300040422     c                   EndIf
044400040422
044500040422     c                   EndSr
044600040422      *------------------------------------------------------------------------*
044700040422      * - ROUTINE INIZIALE
044800040422      *------------------------------------------------------------------------*
044900040422     c     *INZSR        BegSr
045000040422
045100040422     c     *entry        Plist
045200040422     c                   Parm                    kpjba
045300040422
045400040422     c                   Z-ADD     1             CODUT
045500040422     c                   CALL      'X§PARUT'
045600040422     c                   PARM                    UT§DSE0F
045700040422     c                   MOVEL     RAGUT         RSUT
045800040422     c                   MOVEL     REC80         CNCR80
045900040422
046000040422     c     ktab          klist
046100040422     c                   kfld                    codut
046200040422     c                   kfld                    kcod
046300040422     c                   kfld                    kkey
046400040422
046500040422     c     kazcln        klist
046600040422     c                   kfld                    ktfp
046700040422     c                   kfld                    ktfa
046800040422     c                   kfld                    kann
046900040422     c                   kfld                    kmes
047000040422
047100040422     c                   Movel     tit_a         vtctit
047200040422     c                   Clear                   fior44ds
047300040422     c                   Clear                   v1cpoel
047400040422     c                   Clear                   v1dpoel
047500040422
047600040422     c                   If        simtpp = '2' or simtpp = *blanks
047700040422     c                   Movel     simpou        V1Cpoel
047800040422     c                   Eval      *in06 = *On
047900040422     c                   Else
048000040422     c                   Movel     SIMFEL        V1Cpoel
048100040422     c                   EndIf
048200040422
048300040422     c     V1Cpoel       Chain     azorg01L
048400040422     c                   IF        not%found(azorg01l) or
048500040422     c                             orgfva <> *blanks   or
048600040422     c                             (orgfag <> 'A' and orgfag <> 'F')
048700040422     c                   Eval      *in28 = *on
048800040422     c                   Eval      *in40 = *on
048900040422     c                   Movel     msg(1)        v1cmsg
049000040422     c                   ELSE
049100040422      * decodifico p.o. da elaborare
049200040422     c                   Movel     orgDES        v1dpoel
049300040422     c                   ENDIF
049400040422
049500040422      * reperisce data e ora
049600040422     c                   TIME                    W0140
049700040422      * UDATE IN GGMMAAAA
049800040422     c                   MOVE      W0140         WDTGIO
049900040422      * UDATE IN AAAAMMGG
050000040422     c                   Z-ADD     WDTGIO        G02DAT
050100040422     c                   MOVEL     *BLANK        G02ERR
050200040422     c                   CALL      'XSRDA8'
050300040422     c                   PARM                    WLBDAT
050400040422     c                   Z-ADD     G02INV        dataoggi
050500040422     c                   Z-add     g02tgi        sav_g02tgi
050600040422
050700040422     c                   Z-add     1             xa
050800040422
050900040422      * Ricerco tutti i terminal di partenza
051000040422     c                   Clear                   Trul26Ds
051100040422     c                   Eval      I26tpt = 'P'
051200040422     c                   Eval      I26tla = 'L'
051300040422     c                   Call      'TRUL26R'
051400040422     c                   Parm                    Trul26Ds
051500040422      * carico i p.o. da elaborare dell'AS in un sk
051600040422 b1  c                   Do        250           Xb
051700040422     c                   If        Skpot(Xb) = *Zeros
051800040422     c                   Leave
051900040422     c                   EndIf
052000040422      * devo caricare solo quelli che hanno il terminal di partenza = simfel
052100040422     c                   If        Skpot(Xb) = Simfel
052200040422     c                   Eval      Skpoel(Xa) = Skpo(Xb)
052300040422     c                   Add       1             Xa
052400040422     c                   EndIf
052500040422
052600040422 e1  c                   EndDo
052700040422
052800040422      * per filiale
052900040422     c                   Clear                   ds5a2
053000040422     c                   Movel     '5A'          kcod
053100040422     c                   Movel     '2       '    kkey
053200040422     c     ktab          Chain     tabel00f
053300040422     c                   If        %found(tabel00f) and tblflg = *blanks
053400040422     c                   Movel     tbluni        ds5a2
053500040422     c                   Z-add     §5aorf        W0030
053600040422     c                   EndIf
053700040422
053800040422      * Calcolo data
053900040422     c     sav_g02tgi    Sub       W0030         giotgi
054000040422     c                   Call      'XSRGI8'
054100040422     c                   Parm                    wgidat
054200040422     c                   Z-add     giodat        dtpuliggmmaa
054300040422
054400040422      * Giro la data
054500040422     c                   If        dtpuliggmmaa > 0
054600040422     c                   Move      dtpuliggmmaa  g02dat
054700040422     c                   Movel     *blanks       g02err
054800040422     c                   Call      'XSRDA8'
054900040422     c                   Parm                    wlbdat
055000040422     c                   If        g02err = '1'
055100040422     c                   Clear                   dtpuliggmmaa
055200040422     c                   Else
055300040422     c                   Z-add     g02inv        dtpuliaammgg
055400040422     c                   EndIf
055500040422     c                   EndIf
055600040422
055700040422     c                   EndSr
055800040422      *------------------------------------------------------------------------*
055900040422** MSG  Lungh. 78                                                            *
056000070418Filiale non gestita o annullata                                                     1
056100040422Inserire la data immissione                                                         2
056200040422Data immissione errata                                                              3
056300040422Data immissione deve essere inferiore alla data odierna                             4
056400040422Data immissione deve essere maggiore alla data pulizia ORM                          5
056500040422Data immissione deve essere un giorno feriale                                       6
056600040422Non è possibile elaborare un periodo superiore a xx giorni                          7
056700040422Scegliere quale statistica elaborare                                                8
056800040426Immettere l'ordinamento                                                             9
056900040428Immettere il numero di ORM                                                          10
057000040428Immettere il dato solo se richiesta la statistica ORM Telefonici                    11
