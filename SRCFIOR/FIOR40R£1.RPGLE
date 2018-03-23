000100001006     H DECEDIT('0,') DATEDIT(*YMD.)
000200001006
000300001006      *****************************************************************
000400001006      *                                                               *
000500010605      *       STATISTICHE ORM                                         *
000600001006      *                                                               *
000700001006      *****************************************************************
000800001006
000900001010      ****************************************************************
001000001010      *  RIEPILOGO INDICATORI
001100001010      ****************************************************************
001200010618      * 06 - P.O. da elaborare protetto
001300010618      * 12 - Sono in sede
001400010705      * 13 - Utente EDP
001500001010      * 28 - ERRORE GENERICO DSPF
001600001010      * 30 - Comodo
001700010606      * 40 - ERRORE p.o. da elaborare
001800010618      * 41 - ERRORE data elaborazione da
001900010618      * 42 - ERRORE data elaborazione al
002000001010      ****************************************************************
002100001010
002200010605     fazorg01l  if   e           k disk
002300010618     ftabel00f  if   e           k disk
002400010618     fazcln01l  if   e           k disk
002500010605     ffior40d   cf   e             workstn
002600001010
002700001010      *------------------------------------------------------------------------*
002800010625     d worgfel         s                   like(orgfel)
002900010618
003000010618     d kcod            s                   like(tblcod)
003100010618     d kkey            s                   like(tblkey)
003200010618
003300010618     d ktfp            s                   like(clntfp)
003400010618     d ktfa            s                   like(clntfa)
003500010618     d kann            s                   like(clnann)
003600010618     d kmes            s                   like(clnmes)
003700030321
003800030321     d sav_g02tgi      s                   like(g02tgi)
003900010606
004000010618     d xa              s              5i 0 inz(*zeros)                          ind.sk po da elab.
004100020808     d xb              s              5i 0 inz(*zeros)                          ind.sk po TRUL26
004200010618     d wcdtda          s              8  0
004300010618     d wcdtal          s              8  0
004400010618     d da_azorg        s                   like(orgfil)
004500001009
004600010625     d w0030           s              3  0
004700010625     d w0140           s             14  0
004800010605     d wdtgio          s              8  0
004900010618     d dataoggi        s              8  0
005000010622     d dtpuliaammgg    s              8  0
005100010622     d dtpuliggmmaa    s              8  0
005200010618     d wgiorno         s              2  0
005300010618     d wgiolav         s              5  0
005400010622     d datamsg         s             10
005500010705
005600010705     d wprofilo        s              3
005700010618
005800010618     d wdatada         s               d   datfmt(*iso)
005900010618     d wdataal         s               d   datfmt(*iso)
006000001006
006100001010      *  titolo videata (lunghezza massima 34)
006200010605     d tit_a           c                   const('***  Stampa Statistiche O.R.M-
006300010605     d                                     . ***')
006400001031
006500001031      *   S C H I E R E
006600020808     d skpoel          s              3  0 dim(250)                             po da elab.
006700010625     d msg             s             78    dim(9) ctdata perrcd(1)              msg video
006800010605     d l1              s              3  0 dim(30)                              p.o.gestiti
006900001006
007000001010      *   D S   I N T E R N E / E S T E R N E
007100001009
007200010605     d wlbdat          ds                  inz
007300010605     d  g02dat                 1      8  0
007400010605     d  g02inv                 9     16  0
007500010605     d  g02err                17     17
007600010605     d  g02tgi                18     22  0
007700010618
007800010618     d wgidat          ds                  inz
007900010618     d  giodat                 1      8  0
008000010618     d  gioinv                 9     16  0
008100010618     d  giotgi                17     21  0
008200010618
008300010622     d datapul         ds                  inz
008400010622     d  giorno                 1      2
008500010622     d  mese                   3      4
008600010622     d  anno                   5      8
008700010622
008800010618     d clnmat          ds
008900010618     d  mat                    1     31    dim(31)
009000010618     d clnpom          ds
009100010618     d  pom                    1     31    dim(31)
009200001009
009300010605     d fior40ds      e ds
009400010618     d ds5as1        e ds
009500010618     d ds5a2         e ds
009600070418     d  §5astb2      e                     extfld(§5astb)
009700010605     d ut§dse0f      e ds
009800010605     d cncr80        e ds
009900010108
010000020808     d trul26ds      e ds
010100020808     d  skpo                  11    760  0 dim(250)
010200020808     d  skpot                761   1510  0 dim(250)
010300001010
010400010605     d kpjba         e ds
010500001010      *
010600010605     d                sds
010700010605     d  vtcpgm                 1     10
010800001010
010900001006      *------------------------------------------------------------------------*
011000010108
011100010618      * emissione Videata
011200010108
011300010618      * pulizia Videata
011400010618     c                   if        *in28 = *off
011500010606     c                   exsr      Sr_Pulvid
011600010618     c                   endif
011700010606
011800010606     c                   do        *hival
011900010108
012000010605     c                   exfmt     or40d01
012100010108
012200010108      * F3=Fine
012300010605     c   kc              leave
012400010605     c                   setoff                                       28
012500010109
012600010618      * controlli
012700010605     c                   exsr      Sr_Contr
012800010109
012900010109      * F6=Elabora
013000010618     c                   if        *inkf = *on and *in28 = *off
013100010618     c                   exsr      Sr_Contr
013200010618     c  n28              exsr      Sr_Elabora
013300010618     c  n28              leave
013400010618     c                   endif
013500010108
013600010605     c   28              iter
013700010108
013800010605     c                   enddo
013900001010
014000010605     c                   eval      *inlr = *on
014100010108      *****************************************************************
014200010109      * PULIZIA VIDEATA
014300010108      *****************************************************************
014400010605     c     Sr_Pulvid     BEGSR
014500010108
014600010605     c                   clear                   v1cdtda
014700010605     c                   clear                   v1cdtal
014800010618     c                   eval      v1ctpst = 'A'
014900010620     c                   eval      v1cstpc = 'N'
015000010608     c                   eval      v1ctpel = 'S'
015100010705     c                   eval      v1cedp  = 'N'
015200010108
015300010605     c                   endsr
015400010108      **********************************************************************
015500010618      * CONTROLLO I DATI DELLA VIDEATA
015600010108      **********************************************************************
015700010618     c     Sr_Contr      begsr
015800010618
015900010618      * reset indicatori
016000010618     c                   setoff                                       404142
016100010622     c                   setoff                                       43
016200010618
016300010622      * p.o. da elaborare
016400010618     c                   exsr      Sr_Contrpo
016500010618     c                   if        *in28 = *on
016600010618     c                   goto      endsrcontr
016700010618     c                   endif
016800010618
016900010622      * date
017000010618     c                   exsr      Sr_Contrdate
017100010618     c                   if        *in28 = *on
017200010618     c                   goto      endsrcontr
017300010618     c                   endif
017400010622
017500010622      * richiesta lista di controllo
017600010622     c                   if        v1cpoel = 999 and
017700010622     c                             v1cstpc = 'S'
017800010622     c                   seton                                        43  28
017900010622     c                   movel     msg(9)        v1cmsg
018000010622     c                   goto      endsrcontr
018100010622     c                   endif
018200010618
018300010618     c     endsrcontr    endsr
018400010618      **********************************************************************
018500010618      * CONTROLLO IL P.O. DA ELABORARE
018600010618      **********************************************************************
018700010618     c     Sr_Contrpo    begsr
018800010108
018900010618     c                   setoff                                       40
019000010622     c                   clear                   v1dpoel
019100010622
019200010622     c                   if        v1cpoel = 999
019300010622     c                   clear                   v1dpoel
019400010622     c                   eval      v1dpoel = 'Tutti i p.o. dell''AS'
019500010622     c                   goto      endsrctpo
019600010622     c                   endif
019700010622
019800010622      * non deve essere a zero
019900010622     c                   if        v1cpoel = *zeros
020000010622     c                   seton                                        40  28
020100010622     c                   movel     msg(1)        v1cmsg
020200010622     c                   goto      endsrctpo
020300010622     c                   endif
020400010108
020500010625      * deve essere caricato nella sk
020600010618     c     v1cpoel       lookup    skpoel                                 30
020700010618     c                   if        *in30 = *off
020800010618     c                   seton                                        40  28
020900010618     c                   movel     msg(1)        v1cmsg
021000010618     c                   else
021100010108
021200010618      * decodifica e controlla la validità
021300010618     c     v1cpoel       chain     azorg01L
021400010618     c                   if        not%found(azorg01l) or
021500010618     c                             ORGfva <> *blanks
021600010618     c                   seton                                        40  28
021700010618     c                   movel     msg(1)        v1cmsg
021800010618     c                   else
021900010618     c                   if        orgfag <> 'A' and orgfag <> 'F'
022000010618     c                   seton                                        40  28
022100010618     c                   movel     msg(1)        v1cmsg
022200010618     c                   endif
022300010618     c                   movel     orgDES        v1dpoel
022400010618     c                   endif
022500010618     c                   endif
022600010108
022700010622     c     endsrctpo     endsr
022800010618      **********************************************************************
022900010618      * CONTROLLO LE DATE
023000010618      **********************************************************************
023100010618     c     Sr_Contrdate  begsr
023200010618
023300010618     c                   setoff                                       4142
023400010618
023500010618      * data 'da' deve essere impostata
023600010618     c                   if        v1cdtda = *zeros
023700010618     c                   seton                                        41  28
023800010618     c                   movel     msg(2)        v1cmsg
023900010618     c                   goto      endsrctdate
024000010618     c                   endif
024100010618
024200010618      * controllo la data inserita 'da'
024300010618     c                   clear                   wcdtda
024400010618     c                   clear                   wlbdat
024500010618     c                   z-add     v1cdtda       G02dat
024600010618     c                   call      'XSRDA8'
024700010618     c                   parm                    wlbdat
024800010618     c                   if        G02err = '1'
024900010618     c                   eval      *in41 = *ON
025000010618     c                   eval      *in28 = *ON
025100010618     c                   movel     msg(3)        v1cmsg
025200010618     c                   goto      endsrctdate
025300010618     c                   else
025400010618     c                   z-add     G02dat        v1cdtda
025500010618     c                   z-add     G02inv        wcdtda
025600010618     c                   endif
025700010618
025800010618      * non può essere >= ad oggi
025900010618     c                   if        wcdtda >= dataoggi
026000010618     c                   seton                                        41  28
026100010618     c                   movel     msg(4)        v1cmsg
026200010618     c                   goto      endsrctdate
026300010618     c                   endif
026400010618
026500010618      * deve essere > della data di pulizia ORM
026600010622     c                   if        wcdtda <= dtpuliaammgg
026700010618     c                   seton                                        41  28
026800010618     c                   movel     msg(5)        v1cmsg
026900010622     c                   movel     dtpuliggmmaa  datapul
027000010622     c                   eval      datamsg = %trim (giorno + '/' +
027100010622     c                             mese + '/' + anno)
027200010622     c                   eval      %subst(v1cmsg: 62: 10) = datamsg
027300010618     c                   goto      endsrctdate
027400010618     c                   endif
027500010618
027600010618      * deve essere un giorno feriale
027700010618     c     *iso          movel     wcdtda        wdatada
027800010618     c                   extrct    wdatada:*y    kann
027900010618     c                   extrct    wdatada:*m    kmes
028000010618     c                   extrct    wdatada:*d    wgiorno
028100011217     c                   clear                   ktfa
028200010618     c                   if        v1cpoel = 999
028300011217     c                   movel     simfel        ktfp
028400010618     c                   else
028500011217     c                   eval      ktfp = v1cpoel
028600010618     c                   endif
028700010618     c     kazcln        chain     azcln01l
028800010618     c                   if        %found(azcln01l) and
028900010618     c                             (MAT(wgiorno) =  'F'
029000010618     c                                    or  POM(wgiorno) =  'F')
029100010618     c                   seton                                        41  28
029200010622     c                   movel     msg(6)        v1cmsg
029300010618     c                   goto      endsrctdate
029400010618     c                   endif
029500010621      * BLOCCO
029600010621      * non deve essere inferiore a giugno 2001
029700010621     c                   if        kann = 2001 and kmes < 06
029800010621     c                   seton                                        41  28
029900010622     c                   movel     msg(8)        v1cmsg
030000010621     c                   goto      endsrctdate
030100010621     c                   endif
030200010618
030300010618      * data 'al' se non impostata è = a data 'da'
030400010618     c                   if        v1cdtal = *zeros
030500010618     c                   eval      v1cdtal = v1cdtda
030600010618     c                   eval      wcdtal = wcdtda
030700010618     c                   goto      endsrctdate
030800010618
030900010618     c                   else
031000010621
031100010618      * controllo la data inserita 'al'
031200010618     c                   clear                   wcdtal
031300010618     c                   clear                   wlbdat
031400010618     c                   z-add     v1cdtal       G02dat
031500010618     c                   call      'XSRDA8'
031600010618     c                   parm                    wlbdat
031700010618     c                   if        G02err = '1'
031800010618     c                   eval      *in42 = *ON
031900010618     c                   eval      *in28 = *ON
032000010622     c                   movel     msg(3)        v1cmsg
032100010618     c                   goto      endsrctdate
032200010618     c                   else
032300010618     c                   z-add     G02dat        v1cdtal
032400010618     c                   z-add     G02inv        wcdtal
032500010618     c                   endif
032600010618
032700010618      * non può essere >= ad oggi
032800030314      * tranne per sede
032900010618     c                   if        wcdtal >= dataoggi
033000030314     c                             and *in12 = *off
033100010618     c                   seton                                        42  28
033200010622     c                   movel     msg(4)        v1cmsg
033300010618     c                   goto      endsrctdate
033400010618     c                   endif
033500010618
033600010618      * deve essere > della data di pulizia ORM
033700010622     c                   if        wcdtal <= dtpuliaammgg
033800010618     c                   seton                                        42  28
033900010622     c                   movel     msg(5)        v1cmsg
034000010622     c                   movel     dtpuliggmmaa  datapul
034100010622     c                   eval      datamsg = %trim (giorno + '/' +
034200010622     c                             mese + '/' + anno)
034300010622     c                   eval      %subst(v1cmsg: 62: 10) = datamsg
034400010618     c                   goto      endsrctdate
034500010618     c                   endif
034600010621
034700010618      * deve essere un giorno feriale
034800010618     c     *iso          movel     wcdtal        wdataal
034900010618     c                   extrct    wdataal:*y    kann
035000010618     c                   extrct    wdataal:*m    kmes
035100010618     c                   extrct    wdataal:*d    wgiorno
035200011217     c                   clear                   ktfa
035300010618     c                   if        v1cpoel = 999
035400011217     c                   movel     simfel        ktfp
035500010618     c                   else
035600011217     c                   eval      ktfp = v1cpoel
035700010618     c                   endif
035800010618     c     kazcln        chain     azcln01l
035900010618     c                   if        %found(azcln01l) and
036000010618     c                             (MAT(wgiorno) =  'F'
036100010618     c                                    or  POM(wgiorno) =  'F')
036200010618     c                   seton                                        42  28
036300010622     c                   movel     msg(6)        v1cmsg
036400010618     c                   goto      endsrctdate
036500010618     c                   endif
036600010621
036700010621      * BLOCCO
036800010621      * non deve essere inferiore a giugno 2001
036900010621     c                   if        kann = 2001 and kmes < 06
037000010621     c                   seton                                        42  28
037100010622     c                   movel     msg(8)        v1cmsg
037200010621     c                   goto      endsrctdate
037300010621     c                   endif
037400010618
037500010618      * range
037600010618     c                   if        wcdtda > wcdtal
037700010618     c                   seton                                        41  28
037800010622     c                   movel     msg(3)        v1cmsg
037900010618     c                   goto      endsrctdate
038000010618     c                   endif
038100010618
038200010618      * range massimo di 31 gg
038300010726      * dal 26/07/01 range massimo di 7 gg (Bocchi)
038400030307      * 07/03/03 - NICOLA per sede il lancio deve essere possibile per 31 gg
038500010618     c     wdataal       subdur    wdatada       wgiolav:*d
038600010703     c                   eval      wgiolav = wgiolav + 1
038700030307     c                   if        wgiolav > 7 and Not *In12
038800010622     c                   movel     msg(7)        v1cmsg
038900030307     c                   Eval      %Subst(V1cmsg:50:2) = ' 7'
039000010618     c                   seton                                        41  28
039100010618     c                   goto      endsrctdate
039200010618     c                   endif
039300030307     c                   if        wgiolav > 31 and *In12
039400030307     c                   movel     msg(7)        v1cmsg
039500030307     c                   Eval      %Subst(V1cmsg:50:2) = '31'
039600030307     c                   seton                                        41  28
039700030307     c                   goto      endsrctdate
039800030307     c                   endif
039900010618
040000010618     c                   endif
040100010618
040200010618     c     endsrctdate   endsr
040300010606      **********************************************************************
040400010606      * ELABORA
040500010606      **********************************************************************
040600010606     C     Sr_Elabora    begsr
040700010606
040800010606     c                   eval      i40poel = v1cpoel
040900010606
041000010618     c                   eval      i40dtda = wcdtda
041100010618     c                   eval      i40dtal = wcdtal
041200010606
041300010606     c                   eval      i40tpel = v1ctpel
041400010620     c                   eval      i40stpc = v1cstpc
041500010618     c                   eval      i40tpst = v1ctpst
041600010705     c                   eval      i40edp  = v1cedp
041700010621
041800010621     c                   clear                   kpjbu
041900010621     c                   movel     fior40ds      kpjbu
042000010606
042100010618     c                   move      'OR41'        kcoaz
042200011217     c******             call      'FIOR41R'
042300030307     c                   If        *In12
042400030307     c                   call      'BCH09'
042500030307     c                   parm                    kpjba
042600030307     c                   EndIf
042700011217     c                   call      'BCH10'
042800030307     c                   parm                    kpjba
042900010606
043000010618     c                   endsr
043100001010      *****************************************************************
043200001010      * ROUTINE INIZIALE
043300001010      *****************************************************************
043400010618     c     *INZSR        begsr
043500001009
043600010618     c     *entry        plist
043700010618     c                   parm                    kpjba
043800010618
043900010618     c                   Z-ADD     1             CODUT
044000010618     c                   CALL      'X§PARUT'
044100010618     c                   PARM                    UT§DSE0F
044200010618     c                   MOVEL     RAGUT         RSUT
044300010618     c                   MOVEL     REC80         CNCR80
044400010618
044500010618     c     ktab          klist
044600010618     c                   kfld                    codut
044700010618     c                   kfld                    kcod
044800010618     c                   kfld                    kkey
044900010618
045000010618     c     kazcln        klist
045100010618     c                   kfld                    ktfp
045200010618     c                   kfld                    ktfa
045300010618     c                   kfld                    kann
045400010618     c                   kfld                    kmes
045500001011
045600010618     c                   movel     tit_a         vtctit
045700010618     c                   clear                   fior40ds
045800010625     c                   clear                   v1cpoel
045900010625     c                   clear                   v1dpoel
046000010618
046100010618      * da sede non imposto il p.o. da elaborare
046200010625     c                   IF        simfel = *zeros
046300010618     c                   eval      *in12 = *on
046400010618     c                   eval      i40sede = 'S'
046500010625      *
046600010625     c                   ELSE
046700010625      *
046800010618      * da filiale imposto il p.o. da elaborare
046900010618     c                   eval      i40sede = 'F'
047000010108
047100020502     c                   if        simtpp = '2' or simtpp = *blanks
047200020502     c                   movel     simpou        V1Cpoel
047300020808     c                   eval      *in06 = *On
047400010618     c                   else
047500010618     c                   movel     SIMFEL        V1Cpoel
047600010618     c                   endif
047700010625      *
047800010625     c     V1Cpoel       chain     azorg01L
047900010625     c                   IF        not%found(azorg01l) or
048000010625     c                             orgfva <> *blanks   or
048100010625     c                             (orgfag <> 'A' and orgfag <> 'F')
048200010625     c                   eval      *in28 = *on
048300010625     c                   eval      *in40 = *on
048400010625     c                   movel     msg(1)        v1cmsg
048500010625     c                   ELSE
048600010618      * decodifico p.o. da elaborare
048700010618     c                   movel     orgDES        v1dpoel
048800010625     c                   ENDIF
048900010618     c                   endif
049000010705
049100010705      * se profilo EDP richiedo la stampa di controllo
049200010705     c                   movel     knmus         wprofilo
049300010705     c                   eval      *in13 = (wprofilo = 'EDP')
049400001009
049500001023      * reperisce data e ora
049600010618     c                   TIME                    W0140
049700001010      * UDATE IN GGMMAAAA
049800010618     c                   MOVE      W0140         WDTGIO
049900001010      * UDATE IN AAAAMMGG
050000010618     c                   Z-ADD     WDTGIO        G02DAT
050100010618     c                   MOVEL     *BLANK        G02ERR
050200010618     c                   CALL      'XSRDA8'
050300010618     c                   PARM                    WLBDAT
050400010618     c                   Z-ADD     G02INV        dataoggi
050500030321     c                   z-add     g02tgi        sav_g02tgi
050600010618
050700020808     c                   z-add     1             xa
050800010606
050900020808      * Ricerco tutti i terminal di partenza
051000020808     c                   Clear                   Trul26Ds
051100020808     c                   Eval      I26tpt = 'P'
051200020808     c                   Eval      I26tla = 'L'
051300020808     c                   Call      'TRUL26R'
051400020808     c                   Parm                    Trul26Ds
051500020808      * carico i p.o. da elaborare dell'AS in un sk
051600020808 b1  c                   Do        250           Xb
051700020808     c                   If        Skpot(Xb) = *Zeros
051800020808     c                   Leave
051900020808     c                   EndIf
052000020808      * Per la filiale devo caricare solo quelli che hanno il terminal di partenza = simfel
052100020808 b2  c                   If        Not *In12
052200020808     c                   If        Skpot(Xb) = Simfel
052300020808     c                   Eval      Skpoel(Xa) = Skpo(Xb)
052400020808     c                   Add       1             Xa
052500020808     c                   endif
052600020808      * Per la Sede se lancio tutti i p.o. carico tutti
052700020808 x2  c                   Else
052800020808     c                   Eval      Skpoel(Xb) = Skpo(Xb)
052900020808 e2  c                   EndIf
053000020808
053100020808 e1  c                   EndDo
053200010618
053300010618      * cerco data limite lancio elaborazione
053400010618      * per sede
053500010625     c                   IF        *in12 = *on
053600010618     c                   clear                   ds5as1
053700010618     c                   movel     '5A'          kcod
053800010618     c                   movel     'SEDE1'       kkey
053900010618     c     ktab          chain     tabel00f
054000010618     c                   if        %found(tabel00f) and tblflg = *blanks
054100010618     c                   movel     tbluni        ds5as1
054200010625     c                   z-add     §5aors        W0030
054300010618     c                   endif
054400010625      *
054500010625     c                   ELSE
054600010625      * per filiale
054700010625     c                   clear                   ds5a2
054800010625     c                   movel     '5A'          kcod
054900010625     c                   movel     '2       '    kkey
055000010625     c     ktab          chain     tabel00f
055100010625     c                   if        %found(tabel00f) and tblflg = *blanks
055200010625     c                   movel     tbluni        ds5a2
055300010625     c                   z-add     §5aorf        W0030
055400010625     c                   endif
055500010625      *
055600010625     c                   ENDIF
055700010618
055800010618      * Calcolo data
055900030321     c     sav_g02tgi    sub       W0030         giotgi
056000010618     c                   call      'XSRGI8'
056100010618     c                   parm                    wgidat
056200010622     c                   z-add     giodat        dtpuliggmmaa
056300010618
056400010618      * Giro la data
056500010622     c                   if        dtpuliggmmaa > 0
056600010622     c                   move      dtpuliggmmaa  g02dat
056700010618     c                   movel     *blanks       g02err
056800010618     c                   call      'XSRDA8'
056900010618     c                   parm                    wlbdat
057000010618     c                   if        g02err = '1'
057100010625     c                   clear                   dtpuliggmmaa
057200010618     c                   else
057300010622     c                   z-add     g02inv        dtpuliaammgg
057400010618     c                   endif
057500010618     c                   endif
057600030321
057700030321      * se sono in sede
057800030321      * cerco anche quella dei p.o. per elaborare solo gli ORM presenti nei p.o.
057900030321     c                   IF        *in12 = *on
058000030321     c                   clear                   ds5a2
058100030321     c                   movel     '5A'          kcod
058200030321     c                   movel     '2       '    kkey
058300030321     c     ktab          chain     tabel00f
058400030321     c                   if        %found(tabel00f) and tblflg = *blanks
058500030321     c                   movel     tbluni        ds5a2
058600030321     c                   z-add     §5aorf        W0030
058700030321     c                   endif
058800030321      * Calcolo data
058900030321     c     sav_g02tgi    sub       W0030         giotgi
059000030321     c                   call      'XSRGI8'
059100030321     c                   parm                    wgidat
059200030321     c                   z-add     giodat        i40dtpo
059300030321
059400030321      * Giro la data
059500030321     c                   if        i40dtpo > 0
059600030321     c                   move      i40dtpo       g02dat
059700030321     c                   movel     *blanks       g02err
059800030321     c                   call      'XSRDA8'
059900030321     c                   parm                    wlbdat
060000030321     c                   if        g02err = '1'
060100030321     c                   clear                   i40dtpo
060200030321     c                   else
060300030321     c                   z-add     g02inv        i40dtpo
060400030321     c                   endif
060500030321     c                   endif
060600030321     c                   endif
060700010618
060800010618     c                   endsr
060900001010      *---------------------------------------------------------------------------------------------
061000001031** MSG  Lungh. 78                                                            *
061100070418Filiale non gestita o annullata                                                     1
061200010622Inserire la data elaborazione                                                       2
061300010622Data elaborazione errata                                                            3
061400010622Data elaborazione deve essere inferiore alla data odierna                           4
061500010622Data elaborazione deve essere maggiore alla data pulizia ORM                        5
061600010622Data elaborazione deve essere un giorno feriale                                     6
061700030307Non è possibile elaborare un periodo superiore a xx giorni                          7
061800010622Data elaborazione deve essere maggiore del 31 Maggio 2001                           8
061900070418La lista di controllo può essere richiesta se si elabora una sola filiale           9
